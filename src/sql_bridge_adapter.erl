-module(sql_bridge_adapter).


-callback start() -> ok.

-callback connect(DB :: sql_bridge:db(),
				  User :: string(), Pass :: string(),
				  Host :: string(), Port :: integer()) -> ok.

-callback query(Type :: sql_bridge:return_type(),
				DB :: sql_bridge:db(),
				Q :: sql_bridge:sql()) -> {ok, sql_bridge:return_value()} | {error, no_pool | any()}.

-callback query(Type :: sql_bridge:return_type(),
				DB :: sql_bridge:db(),
				Q :: sql_bridge:sql(),
			    ParamList :: [sql_bridge:value()]) -> {ok, sql_bridge:return_value()} | {error, no_pool | any()}.

-callback schema_db_column() -> ColumnName :: string().

-callback encode(V :: any()) -> binary().
