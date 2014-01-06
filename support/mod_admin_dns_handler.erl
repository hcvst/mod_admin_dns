%% @doc DNS Handler for use with the erlang-dns app
%%      https://github.com/hcvst/erlang-dns
%% @author Hans Christian v. Stockhausen <hc@vst.io>
%% @copyright 2014 Hans Christian v. Stockhausen

-module(mod_admin_dns_handler).

-behaviour(ed_gen_handler).

-export([register/1, handle_call/2]).

register(Context) ->
  ed_gen_handler:register_handler(?MODULE, Context).

handle_call(Query, _Context) ->
  error_logger:info_msg("Hello from handle_call"),
  Query.