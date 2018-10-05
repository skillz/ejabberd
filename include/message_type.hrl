%%------------------------------------------------------------------------
%%
%% This file contains message types that we shouldn't include in the 
%% offline message flow.
%%
%% Also contains various constant that we need to access these items.
%%
%%------------------------------------------------------------------------

-define(VsFriendsMessageType, 1).
-define(NudgeMessageType, 3).

-define(NoOfflineToSenderTypes, 
        [
         [?VsFriendsMessageType], 
         [?NudgeMessageType]
        ]).
