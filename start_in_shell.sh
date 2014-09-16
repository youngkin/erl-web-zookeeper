erl -K true +P 10000 -env ERL_MAX_PORTS 10000                               \
	-pa ebin deps/*/ebin                                                   \
	-boot start_sasl                                                       \
	-name erl-web-zk@richsmac.ecollege-dev.com                                      \
	-setcookie COOKIE                                                      \
	-config erl-zookeeper.config                                           \
	-eval "application:ensure_all_started(ez)."
