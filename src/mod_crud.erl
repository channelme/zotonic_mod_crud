%% @author MM Zeeman <maas@channel.me>
%% @copyright 2024 Maas-Maarten Zeeman
%% @doc Easily create, read, update and delete zotonic resources from templates.

%% Copyright 2024 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_crud).

-mod_title("CRUD").
-mod_description("Create, read, update and delete resources from templates.").
-mod_depends([ mod_wires ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([event/2]).

event(#submit{message={create, Args}}, Context) ->
    {cat, Cat} = proplists:lookup(cat, Args),
    CatId = m_rsc:rid(Cat, Context),

    SafeFormValues = proplists:get_all_values(name, Args),
    SafeProps = proplists:get_value(props, Args, #{}),

    %% Only get the pre-defined values from the form to prevent accidental insertion
    %% of values.
    UnsaveFormValues = z_context:get_q_all_noz(Context),
    FormValues = lists:filter(fun({K, _}) -> lists:member(K, SafeFormValues) end, UnsaveFormValues),

    Props = make_props(FormValues, SafeProps, Context),
    Props1 = Props#{ category_id => CatId },

    case m_rsc:insert(Props1, Context) of
        {ok, _Id} ->
            OnSuccess = proplists:get_all_values(on_success, Args),
            z_render:wire(lists:flatten(OnSuccess), Context);
        {error, Reason} ->
            ?LOG_ERROR(#{ text => "Could not create resource",
                          cat => Cat,
                          reason => Reason }),
            growl_error(proplists:get_value(error_message, Args, 
                                           ?__("Sorry, something went wrong.", Context)),
                        Context)
    end;
event(#submit{message={update, Args}}, Context) ->
    {id, Id} = proplists:lookup(id, Args),

    SafeFormValues = proplists:get_all_values(name, Args),
    SafeProps = proplists:get_value(props, Args, #{}),

    %% Only get the pre-defined values from the form to prevent accidental insertion
    %% of values.
    UnsaveFormValues = z_context:get_q_all_noz(Context),
    FormValues = lists:filter(fun({K, _}) -> lists:member(K, SafeFormValues) end, UnsaveFormValues),

    Props = make_props(FormValues, SafeProps, Context),
    Empty = proplists:get_all_values(unlink_when_empty, Args),

    [
     begin
         case proplists:get_value(Key, FormValues, undefined) of
             V when V =:= undefined orelse V =:= <<>> ->
                 m_edge:delete(z_convert:to_integer(Edge), Context);
             _ ->
                 skip
         end
     end || [Key, Edge] <- Empty
    ],

    case m_rsc:update(Id, Props, Context) of
        {ok, _} ->
            OnSuccess = proplists:get_all_values(on_success, Args),
            z_render:wire(lists:flatten(OnSuccess), Context);
        {error, Reason} ->
            ?LOG_ERROR(#{ text => "Could not update resource",
                          id => Id,
                          reason => Reason }),
            growl_error(proplists:get_value(error_message, Args, 
                                           ?__("Sorry, something went wrong.", Context)),
                        Context)
    end;
event(#postback{message={delete, Args}}, Context) ->
    {id, Id} = proplists:lookup(id, Args),

    case m_rsc:delete(Id, Context) of
        ok ->
            OnSuccess = proplists:get_all_values(on_success, Args),
            z_render:wire(lists:flatten(OnSuccess), Context);
        {error, eaccess} ->
            ?LOG_WARNING(#{ text => "Could not delete resource. Not allowed.",
                            reason => eaccess,
                            id => Id }),
            growl_error(proplists:get_value(error_message, Args, 
                                            ?__("Sorry, you are not allowed to delete this resource", Context)),
                        Context);
        {error, Reason} ->
            ?LOG_ERROR(#{ text => "Could not delete resource",
                          id => Id,
                          reason => Reason }),
            growl_error(proplists:get_value(error_message, Args, 
                                            ?__("Sorry, something went wrong.", Context)),
                        Context)
    end.

%%
%% Helpers
%%

growl_error(Msg, Context) ->
    z_render:growl_error(Msg, Context).

make_props(FormValues, Props, _Context) ->
    %% Process the form values, so dates become dt values.
    {ok, MapQsProps} = z_props:from_qs(FormValues),
    maps:merge(Props, MapQsProps).



