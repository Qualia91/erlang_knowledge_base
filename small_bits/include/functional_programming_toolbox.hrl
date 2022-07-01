%%%-----------------------------------------------------------------------------
%%% @title functional_programming_toolbox
%%% @doc
%%% This header file defines a few things to allow you to write in a style akin
%%% to Haskell.
%%%
%%% @author nickw
%%% @copyright MIT
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

%%%=============================================================================
%%% Global Record Definitions
%%%=============================================================================


%%-----------------------------------------------------------------------------
%% @doc
%% The Maybe monad! However the nothing can also hold a value. This can be used
%% to pass error messages about.
%% @end
%%-----------------------------------------------------------------------------
-record(just,       {val :: any()}).
-record(nothing,    {error_message :: binary()}).

-type just(Type)    :: {just,    Type}.
-type nothing() :: {nothing, binary()}.
-type maybe(Type)   :: just(Type) | nothing().
