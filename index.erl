-module(index).
-export([index/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(Debug, true).

debug(X, Y) ->
    case ?Debug of
        true -> io:fwrite(X, Y);
        false -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec index(list({integer(), list(pid())})) -> ok.

index(Catalog) ->
    debug("Catalog is ~w~n", [Catalog]),
    receive
        {getCatalog, Client} ->
            Client!{ok, Catalog},
            index(Catalog);

        {addOwner, ItemId, Peer} ->
            index(addOwner(ItemId, Peer, Catalog))
    end.

addOwner(Id, Owner, []) ->
    [{Id, [Owner]}];

addOwner(Id, Owner, [{Id, Owners}|DB]) ->
    [{Id, [Owner|Owners]}|DB];

addOwner(Id, Owner, [X|DB]) ->
    [X | addOwner(Id, Owner, DB)].
