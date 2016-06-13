create or replace package syntax_tree is
--Copyright (C) 2016 Jon Heller.  This program is licensed under the LGPLv3.

--  _____   ____    _   _  ____ _______   _    _  _____ ______  __     ________ _______ 
-- |  __ \ / __ \  | \ | |/ __ \__   __| | |  | |/ ____|  ____| \ \   / /  ____|__   __|
-- | |  | | |  | | |  \| | |  | | | |    | |  | | (___ | |__     \ \_/ /| |__     | |   
-- | |  | | |  | | | . ` | |  | | | |    | |  | |\___ \|  __|     \   / |  __|    | |   
-- | |__| | |__| | | |\  | |__| | | |    | |__| |____) | |____     | |  | |____   | |   
-- |_____/ \____/  |_| \_|\____/  |_|     \____/|_____/|______|    |_|  |______|  |_|   
-- 
--This package is experimental and does not work yet.


function get_child_node(p_nodes node_table, p_node_index number) return node;
function get_child_node_count(p_nodes node_table, p_node_index number) return number;
function get_child_node_by_type(p_nodes node_table, p_node_index number, p_node_type varchar2, p_occurance number) return node;
procedure add_child_ids(p_nodes in out node_table);

/*

== Purpose ==

Contains functions and procedures for managing node tables - walking, converting, etc.

== Example ==

TODO


*/
end;
/
create or replace package body syntax_tree is
--Copyright (C) 2016 Jon Heller.  This program is licensed under the LGPLv3.

--  _____   ____    _   _  ____ _______   _    _  _____ ______  __     ________ _______ 
-- |  __ \ / __ \  | \ | |/ __ \__   __| | |  | |/ ____|  ____| \ \   / /  ____|__   __|
-- | |  | | |  | | |  \| | |  | | | |    | |  | | (___ | |__     \ \_/ /| |__     | |   
-- | |  | | |  | | | . ` | |  | | | |    | |  | |\___ \|  __|     \   / |  __|    | |   
-- | |__| | |__| | | |\  | |__| | | |    | |__| |____) | |____     | |  | |____   | |   
-- |_____/ \____/  |_| \_|\____/  |_|     \____/|_____/|______|    |_|  |______|  |_|   
-- 
--This package is experimental and does not work yet.


function get_child_node(p_nodes node_table, p_node_index number) return node is
begin
	--TODO
	null;
end;


function get_child_node_count(p_nodes node_table, p_node_index number) return number is
begin
	--TODO
	null;
end;


function get_child_node_by_type(p_nodes node_table, p_node_index number, p_node_type varchar2, p_occurance number) return node is
begin
	--TODO
	null;
end;


--Purpose: Set all the CHILD_IDs of a node_table based on the parent_id.
--ASSUMPTIONS: p_nodes is dense, all child_ids are NULL, all parent_ids are set correctly,
--  nodes are added in tree order so a child node will always have an ID after the parent.
procedure add_child_ids(p_nodes in out node_table) is
	v_child_ids number_table;
begin
	--Loop through each node, look for nodes that refer to it.	
	for i in 1 .. p_nodes.count loop
		v_child_ids := number_table();

		--Gather child nodes.
		for j in i .. p_nodes.count loop
			if p_nodes(j).parent_id = i then
				v_child_ids.extend;
				v_child_ids(v_child_ids.count) := j;
			end if;
		end loop;

		--Set it if it's not null
		if v_child_ids.count > 0 then
			p_nodes(i).child_ids := v_child_ids;
		end if;
			
	end loop;
end;


end;
/
