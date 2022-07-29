from node_definitions import ast_nodes

def write_transform_function(file, definition):
    def write_preamble(file, result_type, syntax_name):
        v = f'[[nodiscard]] static anton::Expected<Owning_Ptr<{result_type}>, Error> transform_{syntax_name}(Allocator* const allocator, Syntax_Node const& node) {{';
        file.write(v)

    def write_member_transform(file, syntax_name, member):
        v = f'''\
Syntax_Node const& {member["name"]}_node = get_{syntax_name}_{member["name"]}(node);
anton::Expected<Owning_Ptr<{member["transform_result"]}>, Error> {member["name"]} = transform_{member["transform_name"]}(allocator, {member["name"]}_node);
if(!{member["name"]}) {{
    return ANTON_MOV({member["name"]});
}}'''
        file.write(v)
        return member["name"]
        
    def write_epilogue(file, ast_name, members):
        members_string = ''
        for member in members:
            members_string += f'ANTON_MOV(*{member}),'
        v = f'return allocate_owning<{ast_name}>(allocator, {members_string} node.source_info);'
        file.write(v)
        file.write("\n}")

    write_preamble(file, definition["result_type"], definition["syntax_name"])
    members = []
    for member in definition["members"]:
        result = write_member_transform(file, definition["syntax_name"], member)
        members.append(result)
    write_epilogue(file, definition["ast_name"], members)

def main():
    file = open("./private/syntax_transforms.hpp", "w")

    for definition in ast_nodes:
        write_transform_function(file, definition)

    file.close()

    process = subprocess.run(["clang-format", "-i", "./private/syntax_transforms.hpp"])
    process.check_returncode()

main()