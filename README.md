# Zotonic CRUD Module
 
Zotonic's data model is very flexible and easy to use from templates. It is 
straightforward to display the author of a resource. Use titles, and even 
custom properties. For a web-designer however it is not easy to create and 
modify resources, because it requires knowledge of [Erlang](https://erlang.org),
the programming language [Zotonic](https://zotonic.com) is written in.

This module makes it possible to savely create, update and delete resources
by using standard web-forms and using zotonic's builtin template language.

# Create

Creating resources from a template with a form can be done with the 
`create` postback.

## `create` postback

| Parameter  | Description                                                            |
|------------|------------------------------------------------------------------------|
| cat        | The category which the new resource must get                           |
| name       | A name of a valid form fields.                                         |
| props      | _Optional_ A pre-defined set of values the new resource must get       |
| on_success | _Optional_ An action which will be performed after the update succeeds |

Example::

```djangohtml
{% wire id=#do_event
        type="submit"
        postback={create cat="event"
                         name="title"
                         name="dt:ymd:0:date_start"
                         name="o.is_going"
                         props = %{
                             is_published: true
                             is_dependent: true
                             date_is_all_day: true
                             content_group: "user_content"
                             "o.author": id
                             "o.is_interested": id
                         }
                         on_success={reload}
        }
        delegate="mod_crud"
%}
```

# Read

Read via `m.rsc`

# Update

## `update` postback

| Parameter         | Description                                                             |
|-------------------|-------------------------------------------------------------------------|
| id                | The id of the resource which must be updated.                           |
| name              | A name of a valid form field                                            |
| on_success        | _Optional_ An action which will be performed after the update succeeds  |
| unlink_when_empty | _Optional_ Make it possible to unlink a resource when an entry is empty |

Example::

```djangohtml
{% wire id=#do_event
        type="submit"
        postback={update id=event_id
                         name="title"
                         name="dt:ymd:0:date_start"
                         name="o.is_going"
                         unlink_when_empty =  ["o.is_going", is_going_edge]
                         on_success={reload}
        }
        delegate="mod_crud"
%}
```

# Delete
 
## `delete` postback


