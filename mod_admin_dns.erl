%% @author Hans Christian v. Stockhausen <hc@vst.io>
%% @copyright 2014 Hans Christian v. Stockhausen

-module(mod_admin_dns).
-author("Hans Christian von Stockhausen <hc@vst.io>").

-mod_title("Admin DNS").
-mod_description("Manage DNS records").
-mod_prio(1000).
-mod_depends([admin]).
-mod_provides([admin_dns]).

-export([
	     init/1,
         observe_admin_menu/3
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


init(Context) ->
    create_database_schema(Context),
    mod_admin_dns_handler:register(Context).

observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_dns,
                parent=admin_content,
                label=?__("Domains", Context),
                url={admin_dns},
                visiblecheck={acl, use, ?MODULE}}
     |Acc].

%%% ==========================================================================
%%% Internal
%%% ==========================================================================

create_database_schema(Context) ->
    case z_db:table_exists(domains, Context) of
        true -> 
            ok;
        false ->
            ok = z_db:transaction(fun install/1, Context),
            error_logger:info_msg("Installed DB schema for mod_admin_dns."),
            z_depcache:flush(Context),
            ok
    end.

install(Context) ->
    z_db:q("
    -- PostgreSQL specific PowerDNS schema 
    -- http://doc.powerdns.com/html/generic-mypgsql-backends.html#idp10675808
       
    CREATE TABLE domains (
      id              SERIAL PRIMARY KEY,
      name            VARCHAR(255) NOT NULL,
      master          VARCHAR(128) DEFAULT NULL,
      last_check      INT DEFAULT NULL,
      type            VARCHAR(6) NOT NULL,
      notified_serial INT DEFAULT NULL,
      account         VARCHAR(40) DEFAULT NULL,
      CONSTRAINT c_lowercase_name CHECK (((name)::text = lower((name)::text)))
    );", Context),

    z_db:q("CREATE UNIQUE INDEX name_index ON domains(name);", Context),

    z_db:q("
    CREATE TABLE records (
      id              SERIAL PRIMARY KEY,
      domain_id       INT DEFAULT NULL,
      name            VARCHAR(255) DEFAULT NULL,
      type            VARCHAR(10) DEFAULT NULL,
      content         VARCHAR(65535) DEFAULT NULL,
      ttl             INT DEFAULT NULL,
      prio            INT DEFAULT NULL,
      change_date     INT DEFAULT NULL,
      CONSTRAINT domain_exists
      FOREIGN KEY(domain_id) REFERENCES domains(id)
      ON DELETE CASCADE,
      CONSTRAINT c_lowercase_name CHECK (((name)::text = lower((name)::text)))
    );", Context),

    z_db:q("CREATE INDEX rec_name_index ON records(name);", Context),

    z_db:q("CREATE INDEX nametype_index ON records(name,type);", Context),

    z_db:q("CREATE INDEX domain_id ON records(domain_id);", Context),

    z_db:q("
    CREATE TABLE supermasters (
      ip INET NOT NULL,
      nameserver VARCHAR(255) NOT NULL,
      account VARCHAR(40) DEFAULT NULL,
      PRIMARY KEY (ip, nameserver)
    );", Context),

    ok.