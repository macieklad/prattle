-module(prattle_utils).

-export([system_message/1]).

system_message(Message) -> "<<prattle>> " ++ Message.
