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
