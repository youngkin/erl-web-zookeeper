erl -K true +P 10000 -env ERL_MAX_PORTS 10000                               \
	-pa ebin deps/*/ebin                                                   \
	-boot start_sasl                                                       \
	-name erl-web-zk@rich.suptest.com                                      \
	-setcookie COOKIE                                                      \
	-config erl-zookeeper.config                                           \
	-eval "application:start(compiler),application:start(syntax_tools),    \
	application:start(goldrush),application:start(lager),                  \
	application:start(ezk), application:start(ranch),                      \
    application:start(crypto), application:start(cowlib),                   \
    application:start(cowboy), application:start(ez)."
