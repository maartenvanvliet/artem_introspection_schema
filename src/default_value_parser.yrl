Nonterminals
  Name  Value EnumValue ListValue Values ObjectValue ObjectFields ObjectField.

Terminals
  '{' '}' '[' ']' ':' 

  name int_value float_value string_value block_string_value boolean_value null.

Rootsymbol Value.

Value -> int_value :     build_ast_node('IntValue',     #{'value' => extract_integer('$1')},             extract_location('$1')).
Value -> float_value :   build_ast_node('FloatValue',   #{'value' => extract_float('$1')},               extract_location('$1')).
Value -> block_string_value :  build_ast_node('StringValue',  #{'value' => extract_quoted_block_string_token('$1')}, extract_location('$1')).
Value -> string_value :  build_ast_node('StringValue',  #{'value' => extract_quoted_string_token('$1')}, extract_location('$1')).
Value -> boolean_value : build_ast_node('BooleanValue', #{'value' => extract_boolean('$1')},             extract_location('$1')).
Value -> null :          build_ast_node('NullValue',    #{},                 extract_location('$1')).
Value -> EnumValue :     build_ast_node('EnumValue',    #{'value' => '$1'},  extract_location('$1')).
Value -> ListValue :     build_ast_node('ListValue',    #{'values' => '$1'}, extract_child_location('$1')).
Value -> ObjectValue :   build_ast_node('ObjectValue',  #{'fields' => '$1'}, extract_child_location('$1')).

Name -> 'name' : '$1'.

EnumValue -> Name : extract_binary('$1').

ListValue -> '[' ']' : [].
ListValue -> '[' Values ']' : '$2'.
Values -> Value : ['$1'].
Values -> Value Values : ['$1'|'$2'].

ObjectValue -> '{' '}' : [].
ObjectValue -> '{' ObjectFields '}' : '$2'.
ObjectFields -> ObjectField : ['$1'].
ObjectFields -> ObjectField ObjectFields : ['$1'|'$2'].
ObjectField -> Name ':' Value : build_ast_node('ObjectField', #{'name' => extract_binary('$1'), 'value' => '$3'}, extract_location('$1')).

Expect 10.

Erlang code.

% Line-Level Utilities

extract_location({_Token, {Line, Column}}) ->
  #{'line' => Line, 'column' => Column};
extract_location({_Token, {Line, Column}, _Value}) ->
  #{'line' => Line, 'column' => Column};
extract_location(_Other) ->
  #{'line' => nil, 'column' => nil}.

extract_child_location([Head|_]) ->
  extract_child_location(Head);
extract_child_location(#{loc := #{'line' := Line, 'column' := Column}}) ->
  #{'line' => Line, 'column' => Column};
extract_child_location(_) ->
  #{'line' => nil, 'column' => nil}.

% Value-level Utilities

extract_binary(Value) when is_binary(Value) ->
  Value;

extract_binary({Token, _Loc}) ->
  list_to_binary(atom_to_list(Token));

extract_binary({_Token, _Loc, Value}) ->
  list_to_binary(Value).

% AST Generation

build_ast_node(Type, Node, #{'line' := nil, 'column' := nil}) ->
  build_ast_node(Type, Node, nil);
build_ast_node(Type, Node, Loc) ->
  'Elixir.Kernel':struct(list_to_atom("Elixir.Absinthe.Language." ++ atom_to_list(Type)), Node#{loc => Loc}).

% String

extract_quoted_string_token({_Token, _Loc, Value}) ->
  unicode:characters_to_binary(lists:sublist(Value, 2, length(Value) - 2)).

% Block String

extract_quoted_block_string_token({_Token, _Loc, Value}) ->
  unicode:characters_to_binary(process_block_string(lists:sublist(Value, 4, length(Value) - 6))).

-spec process_block_string(string()) -> string().
process_block_string(Escaped) ->
  process_block_string(Escaped, []).

-spec process_block_string(string(), string()) -> string().
process_block_string([], Acc) ->
  block_string_value(lists:reverse(Acc));
process_block_string([$\r, $\n | T], Acc) -> process_block_string(T, [$\n | Acc]);
process_block_string([$\\, $", $", $" | T], Acc) -> process_block_string(T, [$", $", $"] ++ Acc);
process_block_string([H | T], Acc) -> process_block_string(T, [H | Acc]).

-spec block_string_value(string()) -> string().
block_string_value(Value) ->
  [FirstLine | Rest] = string:split(Value, "\n", all),
  Prefix = indentation_prefix(common_indent(Rest)),
  UnindentedLines = unindent(Rest, Prefix),
  Lines = trim_blank_lines([FirstLine | UnindentedLines]),
  string:join(Lines, "\n").

-spec trim_blank_lines([string()]) -> [string()].
trim_blank_lines(Lines) ->
  trim_blank_lines(trim_blank_lines(Lines, leading), trailing).

-spec trim_blank_lines([string()], leading | trailing) -> [string()].
trim_blank_lines(Lines, leading) ->
  lists:dropwhile(fun is_blank/1, Lines);
trim_blank_lines(Lines, trailing) ->
  lists:reverse(trim_blank_lines(lists:reverse(Lines), leading)).

-spec indentation_prefix(non_neg_integer()) -> string().
indentation_prefix(Indent) ->
  lists:map(fun(_) -> 32 end, lists:seq(1, Indent)).

-spec unindent([string()], string()) -> [string()].
unindent(Lines, Prefix) ->
  unindent(Lines, Prefix, []).

-spec unindent([string()], string(), [string()]) -> [string()].
unindent([], _Prefix, Result) ->
  lists:reverse(Result);
unindent([H | T], Prefix, Result) ->
  Processed = prefix(H, Prefix),
  unindent(T, Prefix, [Processed | Result]).

-spec prefix(string(), string()) -> string().
prefix(Line, []) ->
  Line;
prefix(Line, Prefix) ->
  Prefixed = lists:prefix(Prefix, Line),
  if
    Prefixed ->
      string:substr(Line, length(Prefix) + 1);
    true ->
      Line
  end.

-spec common_indent([string()]) -> non_neg_integer().
common_indent(Lines) ->
  case common_indent(Lines, noindent) of
    noindent ->
      0;
    Indent ->
      Indent
  end.

-spec common_indent([string()], noindent | non_neg_integer()) -> noindent | non_neg_integer().
common_indent([], Indent) ->
    Indent;
common_indent([H | T], Indent) ->
  CurrentIndent = leading_whitespace(H),
  if
    (CurrentIndent < length(H)) and ((Indent == noindent) or (CurrentIndent < Indent)) ->
      common_indent(T, CurrentIndent);
    true ->
      common_indent(T, Indent)
  end.

-spec leading_whitespace(string()) -> non_neg_integer().
leading_whitespace(BlockStringValue) ->
  leading_whitespace(BlockStringValue, 0).

-spec leading_whitespace(string(), non_neg_integer()) -> non_neg_integer().
leading_whitespace([], N) ->
  N;
leading_whitespace([32 | T], N) ->
  leading_whitespace(T, N + 1);
leading_whitespace([$\t | T], N) ->
  leading_whitespace(T, N + 1);
leading_whitespace([_H | _T], N) ->
  N.

-spec is_blank(string()) -> boolean().
is_blank(BlockStringValue) ->
    leading_whitespace(BlockStringValue) == length(BlockStringValue).

% Integer

extract_integer({_Token, _Loc, Value}) ->
  {Int, []} = string:to_integer(Value), Int.

% Float

extract_float({_Token, _Loc, Value}) ->
  {Float, []} = string:to_float(Value), Float.

% Boolean

extract_boolean({_Token, _Loc, "true"}) ->
  true;
extract_boolean({_Token, _Loc, "false"}) ->
  false.



