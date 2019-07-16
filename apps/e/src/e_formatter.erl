-module(e_formatter).
-export([
  date_to_input/1,
  input_to_date/1
]).

date_to_input(Date) ->
  Date1 = tuple_to_list(Date)
  , Date2 = io_lib:format("~4..0w-~2..0w-~2..0w", Date1)
  , Date3 = iolist_to_binary(Date2)
  , Date3.

input_to_date(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary>>) ->
  Date1 = lists:map(fun binary_to_integer/1, [Y, M, D]),
  Date2 = list_to_tuple(Date1),
  case calendar:valid_date(Date2) of
    true -> Date2;
    false -> error
  end;
input_to_date(_) ->
  error.
