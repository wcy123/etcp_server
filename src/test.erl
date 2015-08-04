-module(test).
-export([
         hi/1
         ]).

hi(A) ->
    error_logger:info_report({1,A}).
