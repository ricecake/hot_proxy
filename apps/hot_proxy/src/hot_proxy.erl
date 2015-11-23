-module(hot_proxy).
-export([
	list_domains/0,
	get_domain/1,
	insert_domain/3,
	remove_domain/1,
	insert_alias/2,
	remove_alias/2,
	insert_host/2,
	remove_host/2,
	update_host/2
]).

list_domains() -> hot_proxy_config:list_domains().

get_domain(Domain) -> hot_proxy_config:get_domain(Domain).

insert_domain(Domain, Aliases, Hosts) ->
	hot_proxy_config:insert_domain(Domain, Aliases, Hosts).

remove_domain(Domain) ->
	hot_proxy_config:remove_domain(Domain).

insert_alias(Domain, Alias) ->
	hot_proxy_config:insert_alias(Domain, Alias).

remove_alias(Domain, Alias) ->
	hot_proxy_config:remove_alias(Domain, Alias).

insert_host(Domain, Host) ->
	hot_proxy_config:insert_host(Domain, Host).

remove_host(Domain, Host) ->
	hot_proxy_config:remove_host(Domain, Host).

update_host(Domain, HostSpec) ->
	hot_proxy_config:update_host(Domain, HostSpec).
