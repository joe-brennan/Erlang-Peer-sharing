-module(peer).
-export([setup/0, peer/2,peerServer/1,lookup/2,peerUser/2,findPeers/2]).

peer(Index, DB) ->
	%% Starts the 'user' part of the peer
  	spawn(?MODULE, peerUser, [Index, self()]),
  	%% Starts the 'file server' part of the peer
  	peerServer(DB).

peerServer(DB) ->
  	receive
    	{add, Id,Content} ->
      	peerServer([{Id, Content} | DB]);

    	{request, Peer, Id} ->
      	%% Calls the lookup function and gets the content from the tuple
			  Result = lookup(Id,DB),

      		if
        		Result == none ->
          		Peer!notFound,
          		peerServer(DB);
			%% If the result is not 'none', get the Value received, and send it back to Peer.
        	true ->
          		Val = element(2, Result),
          		Peer!{found,Id,Val},
          		peerServer(DB)
        end
      end.

-spec lookup(K, list({K, V})) -> none | {some, V}.
%% Looks up an item Id in a peers server, sends the value of the id if found.
lookup(_Id, []) ->
  none;
lookup(Id, [{Id,Value}|_DB]) ->
  {found,Value};
lookup(Id, [{_Key,_Value}|DB]) ->
  lookup(Id, DB).

peerUser(Index, PeerServer) ->
  timer:sleep(2000),

  %% Every two seconds request Catalog from index process
  Index!{getCatalog, self()},
  receive
    {ok, Catalog} ->

      %% Picks a random number from 0-n, n being the size of the catalog, then
      %% gets catalog entry of n. e.g. Random returns {1000,[pid1,pid2,..]}
      RandomEntry = lists:nth(rand:uniform(length(Catalog)), Catalog),
      %% Gets the itemId from the catalog entry, e.g. returns 1000
      RandomId = element(1, RandomEntry),

      %% Find a peer who has the item,
      PeerOwners = findPeers(Catalog,RandomId),

      if
        %% If no peer owns the item, try again by going back to peerUser/2
        PeerOwners == [] ->
          peerUser(Index, PeerServer);

        true ->

          %% If peers found, get a random peer from the list of owners
          RandomPeer = lists:nth(rand:uniform(length(PeerOwners)), PeerOwners),

          %% Check that the peer is not self()
          if
            RandomPeer == PeerServer ->
              peerUser(Index, PeerServer);

            true ->
              %% Check to see if self() already has the item.
              PeerServer!{request, self(), RandomId},

              receive
                %% If self() has the item, go back to peerUser/2 and try again
                {found,_Id,_Content} ->
                  peerUser(Index, PeerServer);
                notFound ->
                  %% Otherwise, requests that item from the random peer
                  RandomPeer!{request, self(),RandomId},
                  receive
                    %% If successful, receive that item from the other peer
                    {found,Id,Content} ->
                      %% Add the item to its own database by sending its own peer server an add message
                      PeerServer!{add,Id,Content},

                      %% Notify the index that the peer server is now an owner of this item.
                      Index!{addOwner,Id,PeerServer},

                      %% Continue updating the catalog and requesting files
                      peerUser(Index, PeerServer);

                    %% If not found. try again.
                    notFound ->
                      peerUser(Index, PeerServer)
                  end
              end
          end
      end
  end.


%% Finds a peer from the catalog who owns an Id given as a parameter. Returns
%% The first peer who owns the Id
findPeers([],_Id) ->
  [];

findPeers([{Id,Peers}|_Catalog],Id) ->
  Peers;
findPeers([{_X,_Peers}|Catalog],Id) ->
  findPeers(Catalog,Id).


%% Setup function which spawns a single indexId
setup() ->
  Index = spawn(index, index, [[]]),
  PeerServer1 = spawn(?MODULE, peer,[Index,[{1000,"item1"}]]),
  PeerServer2 = spawn(?MODULE, peer,[Index,[{1001,"item2"}]]),
  PeerServer3 = spawn(?MODULE, peer,[Index,[{1002,"item3"}]]),
  Index!{addOwner, 1000, PeerServer1},
  Index!{addOwner, 1001, PeerServer2},
  Index!{addOwner, 1002, PeerServer3}.

%% Program Output 1
% Catalog is [{1000,[<0.222.0>,<0.223.0>,<0.221.0>]},{1001,[<0.221.0>,<0.223.0>,<0.222.0>]},{1002,[<0.221.0>,<0.222.0>,<0.223.0>]}]

%% Program Output 2
% Catalog is [{1000,[<0.271.0>,<0.272.0>,<0.270.0>]},{1001,[<0.272.0>,<0.270.0>,<0.271.0>]},{1002,[<0.271.0>,<0.270.0>,<0.272.0>]}]

%% Program Output 3
% Catalog is [{1000,[<0.70.0>,<0.69.0>,<0.68.0>]},{1001,[<0.68.0>,<0.70.0>,<0.69.0>]},{1002,[<0.69.0>,<0.68.0>,<0.70.0>]}]

%% Every time the program is ran, it converges to a situation where every peer owns every file,
%% e.g. [1000,[<X>,<Y>,<Z>],1001,[<Y>,<X>,<Z>],1002,[<Z>,<X>,<Y>]]
