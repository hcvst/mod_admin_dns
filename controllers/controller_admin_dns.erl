%% @author Hans Christian v. Stockhausen <hc@vst.io>
%% @copyright 2014 Hans Christian v. Stockhausen

-module(controller_admin_dns).
-author("Hans Christian von Stockhausen <hc@vst.io>").

-export([
    is_authorized/2
]).

-include_lib("controller_html_helper.hrl").

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_admin_ide, ReqData, Context).


html(Context) ->
    Html = z_template:render("admin_dns.tpl", [], Context),
    z_context:output(Html, Context).

