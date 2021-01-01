%%%-------------------------------------------------------------------
%% @doc Helpers that don't belong to any module specifically
%% @end
%%%-------------------------------------------------------------------
-module(prattle_utils).

-export([system_message/1]).

system_message(Message) -> "<<prattle>> " ++ Message.
