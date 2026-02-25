-module(sql_bridge_key_generator).

-callback generate(sql_bridge:table()) -> sql_bridge:value().
