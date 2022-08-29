Own Beam Language
-----------------

Starting to look at writing my own beam language

## Tips
Compile module to core: erlc +time +to_core FILENAME
The *.core file can be loaded in as normal

# Create lexer
leex:file("likelang.xrl").

# Tokenise a file
{ok, File} = file:read_file("likelang_test_file.lrl").
Content = unicode:characters_to_list(File).
{ok, Tokens, _} = likelang:string(Content).

# Parse Tokens
{ok, SYntaxTree} = yecc:file("likelang.yrl").






leex:file("likelang").
c("likelang.erl").
{ok, File} = file:read_file("likelang_test_file.lrl").
Content = unicode:characters_to_list(File).
likelang:string(Content).