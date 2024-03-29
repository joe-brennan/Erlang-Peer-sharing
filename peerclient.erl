%% Author: Joseph Brennan
%% Date: 28/03/19

-module(peerclient).
-export([setup/0, peer/2,peerServer/1,lookup/2,peerUser/2]).

%% The top-level of a peer
peer(Index, DB) ->
  %% Starts the 'user' part of the peer
  spawn(?MODULE, peerUser, [Index, self()]),
  %% Starts the 'file server' part of the peer
  peerServer(DB).

%% peerServer implementation according to the CFSM given.
peerServer(DB) ->
  receive
    {add, Id,Content} ->
      peerServer([{Id, Content} | DB])

      %% 'add' messages take priority over 'request' messages.
      after 0 ->
        receive
          %% Receive add in the case it has been sent in the time between the priority message
          {add, Id,Content} ->
            peerServer([{Id, Content} | DB]);

          {request, Peer, Id} ->
          %% Calls the lookup function and gets the content from the tuple
          Result = lookup(Id,DB),
          %% Gets the first element of the tuple, will either be found, or none
          if
            %% If the result is not 'none', get the value received, and send it back to Peer.
            Result /= none ->
              %% Gets the second element in the tuple, which is the value
              Val = element(2, Result),
              %% Sends the value found to the peer
              Peer!{found,Id,Val},
              peerServer(DB);

            true ->
              %% Otherwise, send 'notFound' to Peer
              Peer!notFound,
              peerServer(DB)
          end
        end
    end.


%% Looks up an item Id in a peers server, returns the value of the id if found.
-spec lookup(K, list({K, V})) -> none | {some, V}.

%% Uses recursive pattern matching to find the list of tuples
lookup(_Id, []) ->
  none;
lookup(Id, [{Id,Value}|_DB]) ->
  {found,Value};
lookup(Id, [{_Key,_Value}|DB]) ->
  lookup(Id, DB).

%% A peer concurrently requests files from other peers
peerUser(Index, PeerServer) ->
  %% Every two seconds request Catalog from index process
  timer:sleep(2000),
  Index!{getCatalog, self()},

  %% Receive the catalog
  receive
    {ok, Catalog} ->

      %% Pick a random number between 1 and n, n being the size of the catalog, then
      %% get the catalog entry of n. e.g. Random returns {1000,[pid1,pid2,..]}
      RandomEntry = lists:nth(rand:uniform(length(Catalog)), Catalog),

      %% Gets the itemId from the catalog entry, e.g. returns 1000
      RandomId = element(1, RandomEntry),

      %% Gets a list of peers who have the random item,
      PeerOwners = element(2, RandomEntry),

      %% Check to see if self() already has the random item.
      PeerServer!{request, self(), RandomId},
      receive
        %% If the peer already has the item go back to the start of peerUser/2 and try again
        {found,_Id,_Content} ->
          peerUser(Index, PeerServer);

        %% If self() doesn't have the item, request the item from a random peer from the PeerOwners list
        notFound ->
          %% Pick a random number between 1 and n, n being the size of the PeerOwners list, then selects the nth element in the list
          RandomPeer = lists:nth(rand:uniform(length(PeerOwners)), PeerOwners),

          %% Request the item from that peer
          RandomPeer!{request, self(),RandomId},
          receive
            %% Receive the item. User will always successfully receive the item.
            {found,Id,Content} ->
              %% Add the item to own database by sending its own peer server an add message
              PeerServer!{add,Id,Content},
              %% Notify the index that the peer server is now an owner of this item
              Index!{addOwner,Id,PeerServer},
              %% Continue updating the catalog and requesting files by going to the start of peerUser/2
              peerUser(Index, PeerServer)
          end
      end
  end.


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
%% e.g. [{1000,[<X>,<Y>,<Z>]},{1001,[<Y>,<X>,<Z>]},{1002,[<Z>,<X>,<Y>]}]
