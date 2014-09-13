erl -K true +P 10000 -env ERL_MAX_PORTS 10000 							\
	-pa ebin deps/lager/ebin deps/goldrush/ebin deps/ezk/ebin 			\
	-boot start_sasl 													\
	-name suptest@rich.suptest.com 										\
	-setcookie COOKIE 													\
	-config erl-zookeeper.config 										\
	-eval "application:start(compiler),application:start(syntax_tools),	\
	application:start(goldrush),application:start(lager), 				\
	application:start(ezk),application:start(ez)."
