%%------------------------------------------------------------------------
%%
%% This file contains message types that we shouldn't include in the 
%% offline message flow.
%%
%% Also contains various constant values that are the locations of stanza data.
%%
%%------------------------------------------------------------------------

-define(VsFriendsMessageType, 1).
-define(NudgeMessageType, 3).

-define(NoOfflineToSenderTypes, 
        [
         ?VsFriendsMessageType, 
         ?NudgeMessageType
        ]).

-define(SdkElementsPosition, 3).
