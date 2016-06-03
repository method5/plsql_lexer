create or replace package body plsql_parser is
--  _____   ____    _   _  ____ _______   _    _  _____ ______  __     ________ _______ 
-- |  __ \ / __ \  | \ | |/ __ \__   __| | |  | |/ ____|  ____| \ \   / /  ____|__   __|
-- | |  | | |  | | |  \| | |  | | | |    | |  | | (___ | |__     \ \_/ /| |__     | |   
-- | |  | | |  | | | . ` | |  | | | |    | |  | |\___ \|  __|     \   / |  __|    | |   
-- | |__| | |__| | | |\  | |__| | | |    | |__| |____) | |____     | |  | |____   | |   
-- |_____/ \____/  |_| \_|\____/  |_|     \____/|_____/|______|    |_|  |______|  |_|   
-- 
--This package is experimental and does not work yet.



type number_table is table of number;
type string_table is table of varchar2(32767);

g_nodes                     node_table := node_table();
g_ast_tokens                token_table;  --AST = abstract syntax tree.
g_ast_token_index           number;
g_optional                  boolean; --Holds return value of optional functions.
g_parse_tree_tokens         token_table;
g_map_between_parse_and_ast number_table := number_table();
g_reserved_words            string_table;

type parse_context is record
(
	new_node_id number,
	ast_token_index_before number
);

--Temporary constants for ambiguous intermediate nodes that must be resolved later.
C_AMBIG_schema                   constant varchar2(100) := 'C_AMBIG_schema';
C_AMBIG_func_agg_or_analytic     constant varchar2(100) := 'C_AMBIG_func_agg_or_analytic';
C_AMBIG_qn_t_v_mv_alias          constant varchar2(100) := 'C_AMBIG_qn_t_v_mv_alias';
--One of: cluster,column,function,materialized view,operator,package,procedure,query,
--  schema,schema,table,type,view
C_AMBIG_CCFMOPPQSSTTV             constant varchar2(100) := 'C_AMBIG_ccfmoppqssttv';









-------------------------------------------------------------------------------
--Helper functions
-------------------------------------------------------------------------------

--Puprose: Create a new node and return the node ID.
function push(p_node_type in varchar2, p_parent_id in number) return parse_context is
	v_parse_context parse_context;
begin
	g_nodes.extend;
	g_nodes(g_nodes.count) := node(id => g_nodes.count, type => p_node_type, parent_id => p_parent_id, lexer_token => g_ast_tokens(g_ast_token_index));
	v_parse_context.new_node_id := g_nodes.count;
	v_parse_context.ast_token_index_before := g_ast_token_index;
	return v_parse_context;
exception
	when subscript_beyond_count then
		v_parse_context.new_node_id := null;
		v_parse_context.ast_token_index_before := g_ast_token_index;
		return v_parse_context;
end push;


function pop(p_parse_context parse_context) return boolean is
begin
	for i in 1 .. g_nodes.count - (nvl(p_parse_context.new_node_id, g_nodes.count) - 1) loop
		g_nodes.trim;
	end loop;

	g_ast_token_index := p_parse_context.ast_token_index_before;
	return false;
end pop;


function current_value return clob is begin
	begin
		return upper(g_ast_tokens(g_ast_token_index).value);
	exception when subscript_beyond_count then
		return null;
	end;
end current_value;


function current_type return varchar2 is begin
	begin
		return g_ast_tokens(g_ast_token_index).type;
	exception when subscript_beyond_count then
		return null;
	end;
end current_type;


procedure disambig_agg_or_analytic(p_node_type varchar2, p_node_id number) is
begin
	g_nodes(p_node_id).type := p_node_type;
end disambig_agg_or_analytic;


procedure increment(p_increment number default 1) is begin
	g_ast_token_index := g_ast_token_index + p_increment;
end increment;


function match_terminal(p_value varchar2, p_parent_id in number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(p_value, p_parent_id);

	if current_value = p_value then
		increment;
		return true;
	else
		return pop(v_parse_context);
	end if;
end match_terminal;


function next_value(p_increment number default 1) return clob is begin
	begin
		return upper(g_ast_tokens(g_ast_token_index+p_increment).value);
	exception when subscript_beyond_count then
		return null;
	end;
end next_value;


function next_type(p_increment number default 1) return varchar2 is begin
	begin
		return g_ast_tokens(g_ast_token_index+p_increment).type;
	exception when subscript_beyond_count then
		return null;
	end;
end next_type;


function previous_value(p_decrement number) return clob is begin
	begin
		if g_ast_token_index - p_decrement <= 0 then
			return null;
		else
			return upper(g_ast_tokens(g_ast_token_index - p_decrement).value);
		end if;
	exception when subscript_beyond_count then
		null;
	end;
end previous_value;


--Purpose: Determine which reserved words are truly reserved.
--V$RESERVED_WORD.RESERVED is not reliable so we must use dynamic SQL and catch
--errors to build a list of reserved words.
function get_reserved_words return string_table is
	v_dummy varchar2(1);
	v_reserved_words string_table := string_table();
begin
	--Use pre-generated list for specific versions.
	if dbms_db_version.version||'.'||dbms_db_version.release = '12.1' then
		v_reserved_words := string_table(
			'!','!=','$','&','(',')','*','+',',','-','.','/',':',';','<','<<','<=','=','=>',
			'>','>=','?','@','ACCESS','ADD','ALL','ALTER','AND','ANY','AS','ASC','AUDIT',
			'BETWEEN','BY','CHAR','CHECK','CLUSTER','COLUMN','COMMENT','COMPRESS','CONNECT',
			'CREATE','CURRENT','DATE','DECIMAL','DEFAULT','DELETE','DESC','DISTINCT','DROP',
			'ELSE','EXCLUSIVE','EXISTS','FILE','FLOAT','FOR','FROM','GRANT','GROUP','HAVING',
			'IDENTIFIED','IMMEDIATE','IN','INCREMENT','INDEX','INITIAL','INSERT','INTEGER',
			'INTERSECT','INTO','IS','LEVEL','LIKE','LOCK','LONG','MAXEXTENTS','MINUS',
			'MLSLABEL','MODE','MODIFY','NOAUDIT','NOCOMPRESS','NOT','NOWAIT','NULL','NUMBER',
			'OF','OFFLINE','ON','ONLINE','OPTION','OR','ORDER','PCTFREE','PRIOR','PUBLIC',
			'RAW','RENAME','RESOURCE','REVOKE','ROW','ROWID','ROWNUM','ROWS','SELECT',
			'SESSION','SET','SHARE','SIZE','SMALLINT','START','SUCCESSFUL','SYNONYM',
			'SYSDATE','TABLE','THEN','TO','TRIGGER','UID','UNION','UNIQUE','UPDATE','USER',
			'VALIDATE','VALUES','VARCHAR','VARCHAR2','VIEW','WHENEVER','WHERE','WITH','[',
			']','^','{','|','}'
		);
	--TODO: Pre-generate for 11.2
	--Otherwise dynamically determine list.
	else
		for reserved_words in
		(
			select *
			from v$reserved_words
			order by keyword
		) loop
			begin
				execute immediate 'select dummy from dual '||reserved_words.keyword into v_dummy;
			exception when others then
				v_reserved_words.extend;
				v_reserved_words(v_reserved_words.count) := reserved_words.keyword;
				--For testing.
				--dbms_output.put_line('Failed: '||reserved_words.keyword||', Reserved: '||reserved_words.reserved);
			end;
		end loop;
	end if;

	return v_reserved_words;
end get_reserved_words;


--Purpose: Remove the SUBQUERY node, re-number descendents to fill in gap, return parent id. 
--ASSUMPTIONS: 
function remove_extra_subquery(v_subquery_node_id number) return number is
	v_new_nodes node_table := node_table();
begin
	--Copy nodes up until the subquery node.
	for i in 1 .. v_subquery_node_id - 1 loop
		v_new_nodes.extend;
		v_new_nodes(v_new_nodes.count) := g_nodes(i);
	end loop;

	--Copy nodes after subquery until the end.
	for i in v_subquery_node_id + 1 .. g_nodes.count loop
		v_new_nodes.extend;
		--Shrink ID and PARENT_ID by 1 to fill in gap.
		v_new_nodes(v_new_nodes.count) := node(
			id => g_nodes(i).id - 1,
			type => g_nodes(i).type,
			parent_id => g_nodes(i).parent_id - 1,
			lexer_token => g_nodes(i).lexer_token
		);
	end loop;

	--Switcheroo
	g_nodes := v_new_nodes;

	return v_subquery_node_id - 1;
end remove_extra_subquery;


--Purpose: Get the line up to a specific token.
function get_line_up_until_error(p_tokens token_table, p_token_error_index number) return varchar2 is
	v_newline_position number;
	v_line clob;

	--DBMS_INSTR does not allow negative positions so we must loop through to find the last.
	function find_last_newline_position(p_clob in clob) return number is
		v_nth number := 1;
		v_new_newline_position number;
		v_previous_newline_position number;
	begin
		v_previous_newline_position := dbms_lob.instr(lob_loc => p_clob, pattern => chr(10), nth => v_nth);

		loop
			v_nth := v_nth + 1;
			v_new_newline_position := dbms_lob.instr(lob_loc => p_clob, pattern => chr(10), nth => v_nth);

			if v_new_newline_position = 0 then
				return v_previous_newline_position;
			else
				v_previous_newline_position := v_new_newline_position;
			end if;
		end loop;
	end find_last_newline_position;
begin
	--Get text before index token and after previous newline.
	for i in reverse 1 .. p_token_error_index loop
		--Look for the last newline.
		v_newline_position := find_last_newline_position(p_tokens(i).value);

		--Get everything after newline if there is one, and exit.
		if v_newline_position > 0 then
			--(If the last character is a newline, the +1 will return null, which is what we want anyway.)
			v_line := dbms_lob.substr(lob_loc => p_tokens(i).value, offset => v_newline_position + 1) || v_line;
			exit;
		--Add entire string to the line if there was no newline.
		else
			v_line := p_tokens(i).value || v_line;
		end if;
	end loop;

	--Only return the first 4K bytes of data, to fit in SQL varchar2(4000). 
	return substrb(cast(substr(v_line, 1, 4000) as varchar2), 1, 4000);
end get_line_up_until_error;


--Purpose: Raise exception with information about the error.
--ASSUMES: All production rules are coded as functions on a line like: function%
procedure parse_error(p_error_expected_items varchar2, p_line_number number) is
	v_production_rule varchar2(4000);
	v_parse_tree_token_index number;
begin
	--Find the production rule the error line occurred on.
	select production_rule
	into v_production_rule
	from
	(
		--Find the production rule based on the function name.
		--ASSUMES a consistent coding style.
		--(Irony alert - this is exactly the kind of hack this program is built to avoid.)
		select
			row_number() over (order by line desc) last_when_1,
			replace(regexp_replace(text, 'function ([^\(]+).*', '\1'), chr(10)) production_rule
		from user_source
		where name = $$plsql_unit
			and type = 'PACKAGE BODY'
			and line <= p_line_number
			--Assumes coding style.
			and lower(text) like 'function%'
	) function_names
	where last_when_1 = 1;

	--Find the last token examined.
	begin
		v_parse_tree_token_index := g_map_between_parse_and_ast(g_ast_token_index);
	exception when subscript_beyond_count then
		v_parse_tree_token_index := g_map_between_parse_and_ast(g_ast_token_index-1);
	end;

	--Raise an error with some information about the rule.
	raise_application_error(-20123,
		'Error in line '||g_nodes(g_nodes.count).lexer_token.line_number||', '||
		'column '||to_char(g_nodes(g_nodes.count).lexer_token.last_char_position+1)||':'||chr(10)||
		get_line_up_until_error(g_parse_tree_tokens, v_parse_tree_token_index)||'<-- ERROR HERE'||chr(10)||
		'Error in '||v_production_rule||', expected one of: '||p_error_expected_items
	);
--Just in case a function cannot be found.
exception when no_data_found then
	raise_application_error(-20000, 'Could not find function for line number '||p_line_number||'.');
end parse_error;


function match_unreserved_word(node_type varchar2, p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(node_type, p_parent_id);

	if current_type = plsql_lexer.c_word and current_value not member of g_reserved_words then
		increment;
		return true;
	else
		return pop(v_parse_context);
	end if;
end match_unreserved_word;


function is_unreserved_word(p_increment in number) return boolean is
begin
	if next_type(p_increment) = plsql_lexer.c_word and next_value(p_increment) not member of g_reserved_words then
		return true;
	else
		return false;
	end if;
end is_unreserved_word;


--Purpose: Resolve nodes that are ambiguous offline or at the beginning of parsing.
--For example, "select a.* ..." - the "a" can be multiple things, such as a
--table alias, query name, table, view, or a materialized view.
procedure resolve_ambiguous_nodes is
begin
	--TODO
	null;
end resolve_ambiguous_nodes;


--Return the value after the matching parens.
--ASSUMPTION: The current_type is pointing to a "(".
function value_after_matching_parens return clob is
	v_paren_counter number := 1;
begin
	--Only process if starting at '('.
	if next_type(0) = '(' then
		--Loop until a matching ")" is found.
		for token_index in 1 .. (g_ast_tokens.count - g_ast_token_index) loop
			--Increment or decrement counter.
			if next_type(token_index) = '(' then
				v_paren_counter := v_paren_counter + 1;
			elsif next_type(token_index) = ')' then
				v_paren_counter := v_paren_counter - 1;
			end if;

			--Return a value if the counter is 0.
			if v_paren_counter = 0 then
				--If it's the last token, return null;
				if token_index + g_ast_token_index = g_ast_tokens.count then
					return null;
				--Else return the next token type.
				else
					return next_type(token_index+1);
				end if;
			end if;
		end loop;

		--Return null, nothing found
		return null;
	else
		return null;
	end if;
end value_after_matching_parens;





-------------------------------------------------------------------------------
--Production Rules.
-------------------------------------------------------------------------------

--Forward declarations so functions can be placed in alphabetical order.
function containers_clause(p_parent_id number) return boolean;
function flashback_query_clause(p_parent_id number) return boolean;
function for_update_clause(p_parent_id number) return boolean;
function function_expression_1(p_parent_id number) return boolean;
function dblink(p_parent_id number) return boolean;
function else_clause(p_parent_id number) return boolean;
function else_expr(p_parent_id number) return boolean;
function expr(p_parent_id number) return boolean;
function group_by_clause(p_parent_id number) return boolean;
function hierarchical_query_clause(p_parent_id number) return boolean;
function hint(p_parent_id number) return boolean;
function interval_expression(p_parent_id number) return boolean;
function join_clause(p_parent_id number) return boolean;
function model_clause(p_parent_id number) return boolean;
function model_expression(p_parent_id number) return boolean;
function object_access_expression_1(p_parent_id number) return boolean;
function order_by_clause(p_parent_id number) return boolean;
function placeholder_expression(p_parent_id number) return boolean;
function plsql_declarations(p_parent_id number) return boolean;
function query_block(p_parent_id number) return boolean;
function query_partition_clause(p_parent_id number) return boolean;
function query_table_expression(p_parent_id number) return boolean;
function return_expr(p_parent_id number) return boolean;
function row_limiting_clause(p_parent_id number) return boolean;
function searched_case_expression(p_parent_id number) return boolean;
function search_clause(p_parent_id number) return boolean;
function select_list(p_parent_id number) return boolean;
function select_statement(p_parent_id number) return boolean;
function simple_case_expression(p_parent_id number) return boolean;
function simple_expression_1(p_parent_id number) return boolean;
function scalar_subquery_expression(p_parent_id number) return boolean;
function subquery(p_parent_id number) return boolean;
function subquery_factoring_clause(p_parent_id number) return boolean;
function table_reference(p_parent_id number) return boolean;
function type_constructor_expression_1(p_parent_id number) return boolean;
function where_clause(p_parent_id number) return boolean;
function windowing_clause(p_parent_id number) return boolean;
function with_clause(p_parent_id number) return boolean;


--?????
function ambiguous_expression(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
--	v_parse_context.new_node_id := v_parse_context := push(C_AMBIGUOUS_EXPRESSION, p_parent_id);

	--TODO
	return pop(v_parse_context);
end ambiguous_expression;


--Assumption: This is only called where it is required.
--This function always returns true - analytic clauses can be empty.
--For example: select count(*) over () from dual;
function analytic_clause(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_ANALYTIC_CLAUSE, p_parent_id);

	g_optional := query_partition_clause(v_parse_context.new_node_id);
	if order_by_clause(v_parse_context.new_node_id) then
		g_optional := windowing_clause(v_parse_context.new_node_id);
	end if;

	return true;
end analytic_clause;


function argument(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_ARGUMENT, p_parent_id);

	--TODO: Should this be some similar name for different contexts?
	--Sometimes it's ARGUMENT, sometimes it's just expression, etc.
	if expr(v_parse_context.new_node_id) then
		return true;
	else
		return pop(v_parse_context);
	end if;
end argument;


function case_expression(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_CASE_EXPRESSION, p_parent_id);

	if match_terminal('CASE', v_parse_context.new_node_id) then
		if simple_case_expression(v_parse_context.new_node_id) or searched_case_expression(v_parse_context.new_node_id) then
			g_optional := else_clause(v_parse_context.new_node_id);
			if match_terminal('END', v_parse_context.new_node_id) then
				return true;
			else
				parse_error('END', $$plsql_line);
			end if;
		else
			parse_error('simple_case_expression or searched_case_expression', $$plsql_line);
		end if;
	else
		return pop(v_parse_context);
	end if;
end case_expression;


function comparison_expr(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_COMPARISON_EXPR, p_parent_id);

	if expr(v_parse_context.new_node_id) then
		return true;
	else
		return pop(v_parse_context);
	end if;
end comparison_expr;


--This function only covers the easy parts of COMPOUND_EXPRESSION, anything
--that starts with (, +, -, or PRIOR.  Other forms must be handled later.
function compound_expression_1(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_COMPOUND_EXPRESSION, p_parent_id);

	--ASSUMPTION: All other expressions that start with "(" were checked before.
	if match_terminal('(', v_parse_context.new_node_id) then
		if expr(v_parse_context.new_node_id) then
			if match_terminal(')', v_parse_context.new_node_id) then
				return true;
			else
				parse_error('")"', $$plsql_line);
			end if;
		else
			parse_error('expr', $$plsql_line);
		end if;
	elsif match_terminal('+', v_parse_context.new_node_id) or match_terminal('-', v_parse_context.new_node_id) or match_terminal('PRIOR', v_parse_context.new_node_id) then
		if expr(v_parse_context.new_node_id) then
			return true;
		else
			parse_error('expr', $$plsql_line);
		end if;
	else
		return pop(v_parse_context);
	end if;
end compound_expression_1;


function condition(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_CONDITION, p_parent_id);

	--TODO
	return pop(v_parse_context);
end condition;


function containers_clause(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_CONTAINERS_CLAUSE, p_parent_id);

	--TODO
	return pop(v_parse_context);
end containers_clause;


function cursor_expression(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_CURSOR_EXPRESSION, p_parent_id);

	if 
		match_terminal('CURSOR', v_parse_context.new_node_id) and
		match_terminal('(', v_parse_context.new_node_id) and
		subquery(v_parse_context.new_node_id)
	then
		if match_terminal(')', v_parse_context.new_node_id) then
			return true;
		else
			parse_error('")"', $$plsql_line);
		end if;
	else
		return pop(v_parse_context);
	end if;
end cursor_expression;


function cycle_clause(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_CYCLE_CLAUSE, p_parent_id);

	--TODO
	return pop(v_parse_context);
end cycle_clause;


--**DIFFERENCE FROM MANUAL**: The dblink may contain the initial "@".
--It's cleaner to store it in the link instead of in the containing object.
function dblink(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_DBLINK, p_parent_id);

	if match_terminal('@', v_parse_context.new_node_id) then
		if match_unreserved_word('database', v_parse_context.new_node_id) then
			loop
				if match_terminal('.', v_parse_context.new_node_id) then
					if match_unreserved_word('domain', v_parse_context.new_node_id) then
						null;
					else
						parse_error('domain', $$plsql_line);
					end if;
				else
					exit;
				end if;
			end loop;

			if match_terminal('@', v_parse_context.new_node_id) then
				if match_unreserved_word('connection_qualifier', v_parse_context.new_node_id) then
					return true;
				else
					parse_error('connection_qualifier', $$plsql_line);
				end if;
			end if;

			return true;
		else
			parse_error('database', $$plsql_line);
		end if;
	else
		return pop(v_parse_context);
	end if;
end dblink;


--Datetime expressions must be handled after the expressions are created,
--and inserted before the current node.
function datetime_expression(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_DATETIME_EXPRESSION, p_parent_id);

	--TODO
	return pop(v_parse_context);
end datetime_expression;


function else_clause(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_ELSE_CLAUSE, p_parent_id);

	if match_terminal('ELSE', v_parse_context.new_node_id) then
		if else_expr(v_parse_context.new_node_id) then
			return true;
		else
			parse_error('else_expr', $$plsql_line);
		end if;
	else
		return pop(v_parse_context);
	end if;
end else_clause;


function else_expr(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_ELSE_EXPR, p_parent_id);

	if expr(v_parse_context.new_node_id) then
		return true;
	else
		return pop(v_parse_context);
	end if;
end else_expr;


--**MANUAL ERROR**: "variable_expression" should be named "placeholder_expression".
function expr(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_EXPR, p_parent_id);

	/*
	--Ideally expressions would be this simple:
	if
		simple_expression or
		compound_expression or
		case_expression or
		cursor_expression or
		datetime_expression or
		function_expression or
		interval_expression or
		JSON_object_access_expr or
		model_expression or
		object_access_expression or
		scalar_subquery_expression or
		type_constructor_expression or
		placeholder_expression
	then
		return true
	else
		return pop(v_parse_context);
	end if;

	But there's a lot of ambiguity so different expression types must be broken up
	and much of it resolved later.

*/

	if
		case_expression(v_parse_context.new_node_id) or
		cursor_expression(v_parse_context.new_node_id) or
		placeholder_expression(v_parse_context.new_node_id) or
		interval_expression(v_parse_context.new_node_id) or
		model_expression(v_parse_context.new_node_id) or
		scalar_subquery_expression(v_parse_context.new_node_id) or
		simple_expression_1(v_parse_context.new_node_id) or
		object_access_expression_1(v_parse_context.new_node_id) or
		compound_expression_1(v_parse_context.new_node_id) or
		type_constructor_expression_1(v_parse_context.new_node_id) or
		function_expression_1(v_parse_context.new_node_id) or
		--Could be simple_expression,
		ambiguous_expression(v_parse_context.new_node_id)
	then
		g_optional := datetime_expression(v_parse_context.new_node_id);
		return true;
	else
		return pop(v_parse_context);
	end if;
/*
Easy to detect:

	case_expression
	cursor_expression
	placeholder_expression --Manual sometimes calls this variable_expression
	scalar_subquery_expression
	model_expression  --Difference from manual: analytic_function is a function expression
	interval_expression (look for "(", then last ")" then DAY or YEAR
?

	simple_expression  --Problem: missing some literal, missing t_alias.
		easy to detect
		dot and words
	compound_expression
		starts with +, -, PRIOR
		followed by *, /, +, -, ||
		in parens, nothing following it
	datetime_expression
		expression followed by "at" ("local" or "time zone ...")

Just dots and words

	function_expression  --followed by "keep" or "over" or "within"
	JSON_object_access_expr
	object_access_expression   -- ( ... ) "."
	type_constructor_expression  -- may start with "new"

C_AMBIG_expression_objects

simple_expression
	query
	schema
	table
	t_alias
	view
	materialized view
	sequence
function_expression
JSON_object_access_expr
object_access_expression
type_constructor_expression

*/

	return pop(v_parse_context);
end expr;


function flashback_query_clause(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_FLASHBACK_QUERY_CLAUSE, p_parent_id);

	--TODO
	return pop(v_parse_context);
end flashback_query_clause;


function for_update_clause(p_parent_id number) return boolean is
begin
	--TODO
	return true;
end for_update_clause;


--This function only covers the easy parts of FUNCTION_EXPRESSION, anything
--that has a trailing "OVER (", "KEEP (", or "WITHIN GROUP (".  Other function
--expressions are ambiguous and must be handled in post-processing.
function function_expression_1(p_parent_id number) return boolean is
	v_first_parse_context parse_context;
	v_parse_context parse_context;

	--TODO: Make this more global?
	--Assumption: This was called right after a "(".
	procedure arguments is
	begin
		if argument(v_parse_context.new_node_id) then
			loop
				if match_terminal(',', v_parse_context.new_node_id) then
					if argument(v_parse_context.new_node_id) then
						exit;
					else
						parse_error('argument', $$plsql_line);
					end if;
				else
					exit;
				end if;
			end loop;
		elsif match_terminal(')', v_parse_context.new_node_id) then
			null;
		else
			parse_error('argument or ")"', $$plsql_line);
		end if;
	end arguments;

	--Assumption: This was called after an "... ORDER BY" or "," (as part of an order section).
	procedure order_by_item is
	begin
		if expr(v_parse_context.new_node_id) then
			g_optional := match_terminal('DESC', v_parse_context.new_node_id) or match_terminal('ASC', v_parse_context.new_node_id);
			if match_terminal('NULLS', v_parse_context.new_node_id) then
				if match_terminal('FIRST', v_parse_context.new_node_id) or match_terminal('LAST', v_parse_context.new_node_id) then
					null;
				else
					parse_error('FIRST or LAST', $$plsql_line);
				end if;
			end if;
		else
			parse_error('expr', $$plsql_line);
		end if;
	end order_by_item;

	--This OVER is always optional.
	function over_query_partition_clause return boolean is
	begin
		--Match like this because there is no pop.
		--**DIFFERENCE FROM MANUAL**: Manual is missing the "(" and ")" around the query_partition_clause.
		if next_value(0) = 'OVER' and next_value(1) = '(' then
			g_optional := match_terminal('OVER', v_parse_context.new_node_id);
			g_optional := match_terminal('(', v_parse_context.new_node_id);
			if query_partition_clause(v_parse_context.new_node_id) then
				if match_terminal(')', v_parse_context.new_node_id) then
					return true;
				else
					parse_error('")"', $$plsql_line);
				end if;
			else
				parse_error('query_partition_clause', $$plsql_line);
			end if;
		else
			return false;
		end if;
	end over_query_partition_clause;
begin
	v_first_parse_context := push(C_FUNCTION_EXPRESSION, p_parent_id);
	v_parse_context := push(C_AMBIG_func_agg_or_analytic, p_parent_id);

	--Match the function part (words and dots, with parens and links thrown in)
	--
	--First value must be a non-reserved word.
	if match_unreserved_word(C_AMBIG_CCFMOPPQSSTTV, v_parse_context.new_node_id) then

		--Initial Link, link-parens, or parens.
		if dblink(v_parse_context.new_node_id) then
			if match_terminal('(', v_parse_context.new_node_id) then
				g_optional := match_terminal('DISTINCT', v_parse_context.new_node_id) or match_terminal('ALL', v_parse_context.new_node_id);
				arguments;
			end if;
		elsif match_terminal('(', v_parse_context.new_node_id) then
			g_optional := match_terminal('DISTINCT', v_parse_context.new_node_id) or match_terminal('ALL', v_parse_context.new_node_id);
			arguments;
		end if;

		--Series: (DOT WORD (LINK PARENS|LINK|PARENS))*
		loop
			if match_terminal('.', v_parse_context.new_node_id) then
				if match_unreserved_word(C_AMBIG_CCFMOPPQSSTTV, v_parse_context.new_node_id) then
					--Link, link-parens, or parens.
					if dblink(v_parse_context.new_node_id) then
						if match_terminal('(', v_parse_context.new_node_id) then
							g_optional := match_terminal('DISTINCT', v_parse_context.new_node_id) or match_terminal('ALL', v_parse_context.new_node_id);
							arguments;
						end if;
					elsif match_terminal('(', v_parse_context.new_node_id) then
						g_optional := match_terminal('DISTINCT', v_parse_context.new_node_id) or match_terminal('ALL', v_parse_context.new_node_id);
						arguments;
					end if;
				else
					parse_error('unreserved word', $$plsql_line);
				end if;
			else
				exit;
			end if;
		end loop;

		--Aggregate or analytic part.
		if match_terminal('OVER', v_parse_context.new_node_id) and match_terminal('(', v_parse_context.new_node_id) then
			if analytic_clause(v_parse_context.new_node_id) then
				if match_terminal(')', v_parse_context.new_node_id) then
					disambig_agg_or_analytic(C_ANALYTIC_FUNCTION, v_parse_context.new_node_id);
					return true;
				else
					parse_error('")"', $$plsql_line);
				end if;
			else
				parse_error('analytic_clause', $$plsql_line);
			end if;
		elsif match_terminal('WITHIN', v_parse_context.new_node_id) and match_terminal('GROUP', v_parse_context.new_node_id) and match_terminal('(', v_parse_context.new_node_id) then
			if order_by_clause(v_parse_context.new_node_id) then
				if match_terminal(')', v_parse_context.new_node_id) then
					if over_query_partition_clause then
						disambig_agg_or_analytic(C_ANALYTIC_FUNCTION, v_parse_context.new_node_id);
					else
						disambig_agg_or_analytic(C_AGGREGATE_FUNCTION, v_parse_context.new_node_id);
					end if;
					return true;
				else
					parse_error('")"', $$plsql_line);
				end if;
			else
				parse_error('order_by_clause', $$plsql_line);
			end if;
		elsif match_terminal('KEEP', v_parse_context.new_node_id) and match_terminal('(', v_parse_context.new_node_id) then
			if match_terminal('DENSE_RANK', v_parse_context.new_node_id) then
				if match_terminal('FIRST', v_parse_context.new_node_id) or match_terminal('LAST', v_parse_context.new_node_id) then
					if match_terminal('ORDER', v_parse_context.new_node_id) then
						if match_terminal('BY', v_parse_context.new_node_id) then

							order_by_item;

							loop
								if match_terminal(',', v_parse_context.new_node_id) then
									order_by_item;
								else
									exit;
								end if;
							end loop;

							if match_terminal(')', v_parse_context.new_node_id) then
								if over_query_partition_clause then
									disambig_agg_or_analytic(C_ANALYTIC_FUNCTION, v_parse_context.new_node_id);
								else
									disambig_agg_or_analytic(C_AGGREGATE_FUNCTION, v_parse_context.new_node_id);
								end if;
								return true;
							else
								parse_error('")"', $$plsql_line);
							end if;
						else
							parse_error('BY', $$plsql_line);
						end if;
					else
						parse_error('ORDER', $$plsql_line);
					end if;
				else
					parse_error('FIRST or LAST', $$plsql_line);
				end if;
			else
				parse_error('DENSE_RANK', $$plsql_line);
			end if;
		else
			return pop(v_first_parse_context);
		end if;

		return true;
	else
		return pop(v_first_parse_context);
	end if;

end function_expression_1;


function group_by_clause(p_parent_id number) return boolean is
begin
	--TODO
	return true;
end group_by_clause;


function hierarchical_query_clause(p_parent_id number) return boolean is
begin
	--TODO
	return true;
end hierarchical_query_clause;


--Bind variables can be either a non-reserved word or a postive integer (digits only).
function host_variable(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_HOST_VARIABLE, p_parent_id);

	if is_unreserved_word(0) or current_type = plsql_lexer.C_NUMERIC then
		increment;
		return true;
	else
		return pop(v_parse_context);
	end if;
end host_variable;


--Hints are semi-abstract.
--Comments are excluded from the AST tokens because they generally never matter
--and it would really clutter things up to always examine them and ignore them.
--However, a comment in a specific format in the right place should count as a node.
--This means that occasionally we need to search through the parse tokens.
function hint(p_parent_id number) return boolean is
	v_parse_token_index number;
	v_parse_context parse_context;
begin
	v_parse_context := push(C_HINT, p_parent_id);

	--Use "-1" to start at previous node and then iterate forward.
	v_parse_token_index := g_map_between_parse_and_ast(g_ast_token_index-1);
	dbms_output.put_line('Parse token index: '||v_parse_token_index);

	--Start from parse tree token after the last node.
	for i in v_parse_token_index+1 .. g_parse_tree_tokens.count loop
		--False if an abstract token is found
		if g_parse_tree_tokens(i).type not in (plsql_lexer.c_whitespace, plsql_lexer.c_comment, plsql_lexer.c_eof) then
			return pop(v_parse_context);
		--True if it's a hint.
		elsif g_parse_tree_tokens(i).type = plsql_lexer.c_comment and substr(g_parse_tree_tokens(i).value, 1, 3) in ('--+', '/*+') then
			--Replace node that points to abstract token with node that points to comment.
			g_nodes(g_nodes.count) := node(id => g_nodes.count, type => C_HINT, parent_id => p_parent_id, lexer_token => g_parse_tree_tokens(i));
			return true;
		end if;
	end loop;

	return pop(v_parse_context);
exception when subscript_beyond_count then
	return pop(v_parse_context);
end hint;


--Bind variables can be either a non-reserved word or a postive integer (digits only).
function indicator_variable(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_INDICATOR_VARIABLE, p_parent_id);

	if is_unreserved_word(0) or current_type = plsql_lexer.C_NUMERIC then
		increment;
		return true;
	else
		return pop(v_parse_context);
	end if;
end indicator_variable;


function interval_expression(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_INTERVAL_EXPRESSION, p_parent_id);

	--TODO
	return pop(v_parse_context);
end interval_expression;


function join_clause(p_parent_id number) return boolean is
begin
	--TODO
	return true;
end join_clause;



function model_clause(p_parent_id number) return boolean is
begin
	--TODO
	return true;
end model_clause;


function model_expression(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_MODEL_EXPRESSION, p_parent_id);

	--TODO
	return pop(v_parse_context);
end model_expression;


--This function only covers the easy parts of OBJECT_ACCESS_EXPRESSION, anything
--that has a "( ... ) . ".  Other object access  expressions are ambiguous and
--must be handled in post-processing.
--**DIFFERENCE FROM MANUAL**: The attribute/method/arguments can repeat.  For example,
--  think of a method that returns an object that has a method.
function object_access_expression_1(p_parent_id number) return boolean is
	v_parse_context parse_context;

	function attribute_method_args return boolean is
	begin
		if match_terminal('.', v_parse_context.new_node_id) then
			if is_unreserved_word(0) then
				if next_type(1) = '(' then
					g_optional := match_unreserved_word('method', v_parse_context.new_node_id);
					g_optional := match_terminal('(', v_parse_context.new_node_id);
					if argument(v_parse_context.new_node_id) then
						loop
							if match_terminal(',', v_parse_context.new_node_id) then
								if argument(v_parse_context.new_node_id) then
									return null;
								else
									parse_error('argument', $$plsql_line);
								end if;
							else
								exit;
							end if;
						end loop;
					elsif match_terminal(')', v_parse_context.new_node_id) then
						null;
					else
						parse_error('argument or ")"', $$plsql_line);
					end if;

					if not match_terminal(')', v_parse_context.new_node_id) then
						parse_error('")', $$plsql_line);
					end if;
				else
					g_optional := match_unreserved_word('attribute', v_parse_context.new_node_id);
				end if;

				return true;

			else
				parse_error('attribute or method', $$plsql_line);
			end if;
		else
			return false;
		end if;
	end attribute_method_args;

begin
	v_parse_context := push(C_OBJECT_ACCESS_EXPRESSION, p_parent_id);

	if current_type = '(' and value_after_matching_parens = '.' then
		g_optional := match_terminal('(', v_parse_context.new_node_id);
		if expr(v_parse_context.new_node_id) then
			if match_terminal(')', v_parse_context.new_node_id) then
				loop
					exit when not attribute_method_args;
				end loop;
				return true;
			else
				parse_error('")"', $$plsql_line);
			end if;
		else
			parse_error('expr', $$plsql_line);
		end if;
	else
		return pop(v_parse_context);
	end if;
end object_access_expression_1;


function order_by_clause(p_parent_id number) return boolean is
begin
	--TODO
	return true;
end order_by_clause;


function placeholder_expression(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_PLACEHOLDER_EXPRESSION, p_parent_id);

	if match_terminal(':', v_parse_context.new_node_id) then
		if host_variable(v_parse_context.new_node_id) then
			if match_terminal('INDICATOR', v_parse_context.new_node_id) then
				if match_terminal(':', v_parse_context.new_node_id) then
					if indicator_variable(v_parse_context.new_node_id) then
						return true;
					else
						parse_error('indicator_variable', $$plsql_line);
					end if;
				else
					parse_error('":"', $$plsql_line);
				end if;
			elsif match_terminal(':', v_parse_context.new_node_id) then
				if indicator_variable(v_parse_context.new_node_id) then
					return true;
				else
					parse_error('indicator_variable', $$plsql_line);
				end if;
			else
				return true;
			end if;
		else
			parse_error('host_variable', $$plsql_line);
		end if;
	else
		return pop(v_parse_context);
	end if;
end placeholder_expression;


function plsql_declarations(p_parent_id number) return boolean is
begin
	--TODO: PL/SQL is not yet supported.
	if current_value in ('PROCEDURE', 'FUNCTION') and next_value not in ('(', 'AS') then
		raise_application_error(-20000, 'PL/SQL is not yet supported.');
	else
		return false;
	end if;
end plsql_declarations;


function query_block(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_QUERY_BLOCK, p_parent_id);
	g_optional := with_clause(v_parse_context.new_node_id);
	if match_terminal('SELECT', v_parse_context.new_node_id) then
		g_optional := hint(v_parse_context.new_node_id);
		g_optional := match_terminal('DISTINCT', v_parse_context.new_node_id) or match_terminal('UNIQUE', v_parse_context.new_node_id) or match_terminal('ALL', v_parse_context.new_node_id);
		if select_list(v_parse_context.new_node_id) then
			if match_terminal('FROM', v_parse_context.new_node_id) then
				if
				(
					table_reference(v_parse_context.new_node_id) or
					join_clause(v_parse_context.new_node_id) or
					(
						match_terminal('(', v_parse_context.new_node_id) and
						join_clause(v_parse_context.new_node_id) and
						match_terminal(')', v_parse_context.new_node_id)
					)
				) then
					loop
						if match_terminal(',', v_parse_context.new_node_id) then
							if
							(
								table_reference(v_parse_context.new_node_id) or
								join_clause(v_parse_context.new_node_id) or
								(
									match_terminal('(', v_parse_context.new_node_id) and
									join_clause(v_parse_context.new_node_id) and
									match_terminal(')', v_parse_context.new_node_id)
								)
							) then
								null;
							else
								parse_error('table_reference, join_clause, or ( join_clause )', $$plsql_line);
							end if;
						else
							exit;
						end if;
					end loop;

					g_optional := where_clause(v_parse_context.new_node_id);
					g_optional := hierarchical_query_clause(v_parse_context.new_node_id);
					g_optional := group_by_clause(v_parse_context.new_node_id);
					g_optional := model_clause(v_parse_context.new_node_id);
					return true;
				else
					parse_error('table_reference, join_clause, or ( join_clause )', $$plsql_line);
				end if;
			else
				parse_error('FROM', $$plsql_line);
			end if;
		else
			parse_error('select_list', $$plsql_line);
		end if;
	else
		return pop(v_parse_context);
	end if;
end query_block;


function query_partition_clause(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_QUERY_PARTITION_CLAUSE, p_parent_id);

	--TODO
	return pop(v_parse_context);
end query_partition_clause;


function query_table_expression(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_QUERY_TABLE_EXPRESSION, p_parent_id);

	--TODO:
	--lateral, table_collection_expression, schema., etc.

	if match_unreserved_word(C_QUERY_NAME, v_parse_context.new_node_id) then
		return true;
	end if;

	return pop(v_parse_context);
end query_table_expression;


function return_expr(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_RETURN_EXPR, p_parent_id);

	if expr(v_parse_context.new_node_id) then
		return true;
	else
		return pop(v_parse_context);
	end if;
end return_expr;


function row_limiting_clause(p_parent_id number) return boolean is
begin
	--TODO
	return true;
end row_limiting_clause;


function scalar_subquery_expression(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_SCALAR_SUBQUERY_EXPRESSION, p_parent_id);

	if current_value = '(' and subquery(v_parse_context.new_node_id) then
		return true;
	else
		return pop(v_parse_context);
	end if;
end scalar_subquery_expression;


function searched_case_expression(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_SEARCHED_CASE_EXPRESSION, p_parent_id);

	if match_terminal('WHEN', v_parse_context.new_node_id) then
		if condition(v_parse_context.new_node_id) then
			if match_terminal('THEN', v_parse_context.new_node_id) then
				if return_expr(v_parse_context.new_node_id) then

					loop
						if match_terminal('WHEN', v_parse_context.new_node_id) then
							if condition(v_parse_context.new_node_id) then
								if match_terminal('THEN', v_parse_context.new_node_id) then
									if return_expr(v_parse_context.new_node_id) then
										null;
									else
										parse_error('return_expr', $$plsql_line);
									end if;
								else
									parse_error('THEN', $$plsql_line);
								end if;
							else
								parse_error('comparison_expr', $$plsql_line);
							end if;
						else
							exit;
						end if;
					end loop;
					return true;

				else
					parse_error('return_expr', $$plsql_line);
				end if;
			else
				parse_error('THEN', $$plsql_line);
			end if;
		else
			parse_error('comparison_expr', $$plsql_line);
		end if;
	else
		return pop(v_parse_context);
	end if;

end searched_case_expression;


function search_clause(p_parent_id number) return boolean is
begin
	--TODO
	return true;
end search_clause;


--select::=
--**DIFFERENCE FROM MANUAL**: "select_statement" instead of "select" to avoid collision with SELECT token.
function select_statement(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_SELECT_STATEMENT, p_parent_id);

	if subquery(v_parse_context.new_node_id) then
		g_optional := for_update_clause(v_parse_context.new_node_id);
		--**DIFFERENCE FROM MANUAL**: The semicolon is optional, not required.
		g_optional := match_terminal(';', v_parse_context.new_node_id);
		return true;
	else
		return pop(v_parse_context);
	end if;
end select_statement;


function select_list(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_SELECT_LIST, p_parent_id);

	--**DIFFERENCE FROM MANUAL**:
	--The top "t_alias.*" in the manual is incorrect.
	--It implies a table alias can only be used once when it can be used many times.
	--For example, this is a valid query: select a.*, b.* from dual a, dual b;
	--Accordingly, the "t_alias.*" is moved a bit and is an alternative to query_name and schema.table|view|materialized_view.
	if match_terminal('*', v_parse_context.new_node_id) then
		return true;
	elsif is_unreserved_word(0) and next_type(1) = '.' and next_type(2) = '*' then
		g_optional := match_unreserved_word(C_AMBIG_qn_t_v_mv_alias, v_parse_context.new_node_id) and match_terminal('.', v_parse_context.new_node_id) and match_terminal('*', v_parse_context.new_node_id);
	elsif is_unreserved_word(0) and next_type(1) = '.' and is_unreserved_word(2) and next_type(3) = '.' and next_type(4) = '*' then
		g_optional := match_unreserved_word(C_AMBIG_schema, v_parse_context.new_node_id) and match_terminal('.', v_parse_context.new_node_id) and match_unreserved_word(C_AMBIG_qn_t_v_mv_alias, v_parse_context.new_node_id) and match_terminal('.', v_parse_context.new_node_id) and match_terminal('*', v_parse_context.new_node_id);
	elsif expr(v_parse_context.new_node_id) then
		if match_terminal('AS', v_parse_context.new_node_id) then
			if match_unreserved_word(C_ALIAS, v_parse_context.new_node_id) then
				null;
			else
				parse_error('c_alias', $$plsql_line);
			end if;
		else
			g_optional := match_unreserved_word(C_ALIAS, v_parse_context.new_node_id);
		end if;
	else
		return pop(v_parse_context);
	end if;

	loop
		if match_terminal(',', v_parse_context.new_node_id) then
			if is_unreserved_word(0) and next_type(1) = '.' and next_type(2) = '*' then
				g_optional := match_unreserved_word(C_AMBIG_qn_t_v_mv_alias, v_parse_context.new_node_id) and match_terminal('.', v_parse_context.new_node_id) and match_terminal('*', v_parse_context.new_node_id);
			elsif is_unreserved_word(0) and next_type(1) = '.' and is_unreserved_word(2) and next_type(3) = '.' and next_type(4) = '*' then
				g_optional := match_unreserved_word(C_AMBIG_schema, v_parse_context.new_node_id) and match_terminal('.', v_parse_context.new_node_id) and match_unreserved_word(C_AMBIG_qn_t_v_mv_alias, v_parse_context.new_node_id) and match_terminal('.', v_parse_context.new_node_id) and match_terminal('*', v_parse_context.new_node_id);
			elsif expr(v_parse_context.new_node_id) then
				if match_terminal('AS', v_parse_context.new_node_id) then
					if match_unreserved_word(C_ALIAS, v_parse_context.new_node_id) then
						null;
					else
						parse_error('c_alias', $$plsql_line);
					end if;
				else
					g_optional := match_unreserved_word(C_ALIAS, v_parse_context.new_node_id);
				end if;
			else
				parse_error('t_alias.*, query_name.*, schema.table|view|materialized view.*, or expr', $$plsql_line);
			end if;
		else
			exit;
		end if;
	end loop;

	return true;

end select_list;


function simple_case_expression(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_SIMPLE_CASE_EXPRESSION, p_parent_id);

	if expr(v_parse_context.new_node_id) then
		if match_terminal('WHEN', v_parse_context.new_node_id) then
			if comparison_expr(v_parse_context.new_node_id) then
				if match_terminal('THEN', v_parse_context.new_node_id) then
					if return_expr(v_parse_context.new_node_id) then

						loop
							if match_terminal('WHEN', v_parse_context.new_node_id) then
								if comparison_expr(v_parse_context.new_node_id) then
									if match_terminal('THEN', v_parse_context.new_node_id) then
										if return_expr(v_parse_context.new_node_id) then
											null;
										else
											parse_error('return_expr', $$plsql_line);
										end if;
									else
										parse_error('THEN', $$plsql_line);
									end if;
								else
									parse_error('comparison_expr', $$plsql_line);
								end if;
							else
								exit;
							end if;
						end loop;
						return true;

					else
						parse_error('return_expr', $$plsql_line);
					end if;
				else
					parse_error('THEN', $$plsql_line);
				end if;
			else
				parse_error('comparison_expr', $$plsql_line);
			end if;
		else
			parse_error('WHEN', $$plsql_line);
		end if;
	else
		return pop(v_parse_context);
	end if;
end simple_case_expression;


--This function only covers the easy parts of SIMPLE_EXPRESSION, basically everything
--except for query_name, schema, table, view, materialized view.  Those are
--ambiguous and must be handled in post-processing.
--**DIFFERENCE FROM MANUAL**: Numeric literals do not include "+" and "-".  Those are expressions.
--**DIFFERENCE FROM MANUAL**: Date, timestamp, and interval are stored as simple expressions.
--**DIFFERENCE FROM MANUAL**: Timestamps are all lumped together.  "WITH TIME ZONE"
--and "WITH LOCAL TIME ZONE" are all timestamps.
function simple_expression_1(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_SIMPLE_EXPRESSION, p_parent_id);

	if match_terminal('ROWNID', v_parse_context.new_node_id) then
		return true;
	elsif match_terminal('ROWNUM', v_parse_context.new_node_id) then
		return true;
	elsif current_type = plsql_lexer.C_TEXT then
		v_parse_context := push(C_STRING, v_parse_context.new_node_id);
		increment;
		return true;
	elsif current_type = plsql_lexer.C_NUMERIC then
		v_parse_context := push(C_NUMBER, v_parse_context.new_node_id);
		increment;
		return true;
	elsif match_terminal('NULL', v_parse_context.new_node_id) then
		return true;
	elsif match_terminal('DATE', v_parse_context.new_node_id) then
		if current_type = plsql_lexer.C_TEXT then
			increment;
			return true;
		else
			parse_error('date string', $$plsql_line);
		end if;
	elsif match_terminal('TIMESTAMP', v_parse_context.new_node_id) then
		if current_type = plsql_lexer.C_TEXT then
			increment;
			return true;
		else
			parse_error('timestamp string', $$plsql_line);
		end if;
	--TODO: interval
	else
		return pop(v_parse_context);
	end if;
end simple_expression_1;


function subquery(p_parent_id number) return boolean is
	v_parse_context parse_context;
	v_second_parse_context parse_context;
begin
	v_parse_context := push(C_SUBQUERY, p_parent_id);

	--Third branch of diagram.
	if match_terminal('(', v_parse_context.new_node_id) then 
		if subquery(v_parse_context.new_node_id) then
			if match_terminal(')', v_parse_context.new_node_id) then
				--Two optional rules at the end. 
				g_optional := order_by_clause(v_parse_context.new_node_id);
				g_optional := row_limiting_clause(v_parse_context.new_node_id);
				return true;
			else
				parse_error('")"', $$plsql_line);
			end if;
		else
			--???? Two pops?
			return pop(v_parse_context);
		end if;

	--First or second branch of diagram.
	else
		--Assume it's a subquery (middle branch) - workaround to avoid left-recursion.
		v_second_parse_context := push(C_SUBQUERY, v_parse_context.new_node_id);

		if query_block(v_second_parse_context.new_node_id) then
			--Second branch of diagram.
			if current_value in ('UNION', 'INTERSECT', 'MINUS') then
				loop
					if
					(
						(match_terminal('UNION', v_parse_context.new_node_id) and match_terminal('ALL', v_parse_context.new_node_id) is not null)
						or
						match_terminal('INTERSECT', v_parse_context.new_node_id)
						or
						match_terminal('MINUS', v_parse_context.new_node_id)
					) then
						if subquery(v_parse_context.new_node_id) then
							null;
						else
							parse_error('subquery', $$plsql_line);
						end if;
					else
						exit when true;						
					end if;
				end loop;
				return true;
			--First branch of diagram.
			else
				--Remove extra SUBQUERY, it's a plain QUERY_BLOCK.
				v_parse_context.new_node_id := remove_extra_subquery(v_second_parse_context.new_node_id);

				--Two optional rules at the end. 
				g_optional := order_by_clause(v_parse_context.new_node_id);
				g_optional := row_limiting_clause(v_parse_context.new_node_id);
				return true;
			end if;
		else
			return pop(v_parse_context);
		end if;
	end if;
end subquery;


function subquery_factoring_clause(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_SUBQUERY_FACTORING_CLAUSE, p_parent_id);

	if match_unreserved_word(C_QUERY_NAME, v_parse_context.new_node_id) then
		if match_terminal('(', v_parse_context.new_node_id) then
			if match_unreserved_word(C_ALIAS, v_parse_context.new_node_id) then
				loop
					if match_terminal('(', v_parse_context.new_node_id) then
						if match_unreserved_word(C_ALIAS, v_parse_context.new_node_id) then
							null;
						else
							parse_error('C_ALIAS', $$plsql_line);
						end if;
					else
						exit;
					end if;
				end loop;
			end if;
		end if;

		if match_terminal('AS', v_parse_context.new_node_id) then
			if match_terminal('(', v_parse_context.new_node_id) then
				if subquery(v_parse_context.new_node_id) then
					if match_terminal(')', v_parse_context.new_node_id) then
						g_optional := search_clause(v_parse_context.new_node_id);
						g_optional := cycle_clause(v_parse_context.new_node_id);
						return true;
					else
						parse_error('")"', $$plsql_line);
					end if;
				else
					parse_error('subquery', $$plsql_line);
				end if;
			else
				parse_error('"("', $$plsql_line);
			end if;
		else
			parse_error('AS', $$plsql_line);
		end if;
	end if;

	return pop(v_parse_context);
end subquery_factoring_clause;


function table_reference(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_TABLE_REFERENCE, p_parent_id);

	if containers_clause(v_parse_context.new_node_id) then
		g_optional := match_unreserved_word(C_T_ALIAS, v_parse_context.new_node_id);
	elsif next_value(1) = 'ONLY' and next_value(2) = '(' then
		increment;
		increment;
		if query_table_expression(v_parse_context.new_node_id) then
			if match_terminal(')', v_parse_context.new_node_id) then
--				g_optional := flashback_query_clause;
--				g_optional := pivot_clause or unpivot_clause or row_pattern_clause;
				g_optional := match_unreserved_word(C_T_ALIAS, v_parse_context.new_node_id);
				return true;				
			else
				parse_error('")"', $$plsql_line);
			end if;
		else
			parse_error('query_table_expression', $$plsql_line);
			return pop(v_parse_context);
		end if;
	elsif query_table_expression(v_parse_context.new_node_id) then
		g_optional := flashback_query_clause(v_parse_context.new_node_id);
--		g_optional := pivot_clause or unpivot_clause or row_pattern_clause;
		g_optional := match_unreserved_word(C_T_ALIAS, v_parse_context.new_node_id);
		return true;
	else
		parse_error('ONLY(query_table_expression), query_table_expression, or containers_clause', $$plsql_line);
		return pop(v_parse_context);
	end if;
end table_reference;


--This function only covers the easy parts of TYPE_CONSTRUCTOR_EXPRESSION, anything
--that has a "new WORD ( ...".  Other type constructor expressions are ambiguous and
--must be handled in post-processing.
function type_constructor_expression_1(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_TYPE_CONSTRUCTOR_EXPRESSION, p_parent_id);

	--TODO
	return pop(v_parse_context);
end type_constructor_expression_1;


function where_clause(p_parent_id number) return boolean is
begin
	--TODO
	return true;
end where_clause;


function windowing_clause(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_WINDOWING_CLAUSE, p_parent_id);

	--TODO
	return pop(v_parse_context);
end windowing_clause;


function with_clause(p_parent_id number) return boolean is
	v_parse_context parse_context;
begin
	v_parse_context := push(C_WITH_CLAUSE, p_parent_id);

	if match_terminal('WITH', v_parse_context.new_node_id) then
		--**DIFFERENCE FROM MANUAL**  (sort of, it matches the "Note")
		--"Note:
		--You cannot specify only the WITH keyword. You must specify at least one of the clauses plsql_declarations or subquery_factoring_clause."
		if not (plsql_declarations(v_parse_context.new_node_id) or subquery_factoring_clause(v_parse_context.new_node_id)) then
			parse_error('plsql_declarations or subquery_factoring_clause', $$plsql_line);
		else
			return true;
		end if;
	else
		return pop(v_parse_context);
	end if;
end with_clause;










-------------------------------------------------------------------------------
--Main Function
-------------------------------------------------------------------------------
/*
	Purpose: Recursive descent parser for PL/SQL.

	This link has a good introduction to recursive descent parsers: https://www.cis.upenn.edu/~matuszek/General/recursive-descent-parsing.html)
*/
function parse(
		p_source        in clob,
		p_user          in varchar2 default user
) return node_table is
begin
	--Check input.
	--TODO

	--Conditional compilation?
	--TODO

	--Reset values, tokenize input.
	g_nodes := node_table();
	g_ast_tokens := token_table();
	g_ast_token_index := 1;
	g_parse_tree_tokens := plsql_lexer.lex(p_source);
	if g_reserved_words is null then
		g_reserved_words := get_reserved_words;
	end if;

	--Convert parse tree into abstract syntax tree by removing whitespace, comment, and EOF.
	--Also create a map between the two.
	for i in 1 .. g_parse_tree_tokens.count loop
		if g_parse_tree_tokens(i).type not in (plsql_lexer.c_whitespace, plsql_lexer.c_comment, plsql_lexer.c_eof) then
			g_ast_tokens.extend;
			g_ast_tokens(g_ast_tokens.count) := g_parse_tree_tokens(i);

			g_map_between_parse_and_ast.extend;
			g_map_between_parse_and_ast(g_map_between_parse_and_ast.count) := i;
		end if;
	end loop;

	--Classify, create statement based on classification.
	g_optional := select_statement(null);

	--Throw error if any tokens remain.
	if current_value is not null then
		parse_error('<empty token>', $$plsql_line);
	end if;

	--Second-pass to resolve ambiguous nodes.
	resolve_ambiguous_nodes;

	return g_nodes;
end parse;

end;
/