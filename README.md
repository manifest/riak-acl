# Riak ACL

[![Build Status][travis-img]][travis]

Access control list (ACL) on top of Riak KV


### Why you might consider to use this library

- Reliability.
	By default, strict quorum (pr=quorum) of vnodes is required for
	create or update operations on ACL entries. You would always know if operation is failed.
- Availability.
	By default, sloppy quorum of vnodes is used for read operations on ACL entries.
	You would able to authorize user's requests even when all but one vnode fail.
- Horizontal scalability.
	Being based on Riak KV, the number of ACL entries aren't important anymore.
- High performance.
	Up to two requests 'by key' are performed for authorize operations.
- Flexibility.
	It's possible to limit: time of subject's access to an object or time that object itself
	is available. Or take access to an object back from a subject any time you need it.
	It's on your own to choose where to store ACL entries of subjects and objects
	by specifying bucket names of Riak KV.
- Extensibility.
	Custom ACL entries could be used by implementing `riakacl` behavior.
- Optional support for Solr-based Riak search 2.0.
	If you need to perform queries through entire ACL data, schemas are available.


### Overview

Riak ACL is an extendable authorization framework. Authorization based on ACL stored in Riak KV.
An object of authorization is an entity the access is need to be granted.
An subject of authorization is an entity that is aiming to get an access to the object.
An ACL entry of the subject contains a list of groups it member of.
The membership in each group can be limited in time.
An ACL entry of the object also cantains a list of groups it member of.
For each of them, access rules and the expiration time are specified.

An subject gets an access to an object only if all three conditions are met:
- the subject and the object are members of the same group;
- time of the subject's membership in the group isn't expired;
- time of the access to the object isn't expired.

*NOTE: expired groups of subject or object are removed automatically when it gets updated in Riak KV.*

![riak-acl-groups][riak-acl-groups-img]

Suppose we have an subject that is a member of the groups:
- A<sub>s+</sub>, access isn't limited in time;
- B<sub>s1</sub>, access expires at time 1;

And an object that is a member of the groups:
- A<sub>o2 r-</sub>, access expires at time 2, group members have **read** access;
- B<sub>o3 -w</sub>, access expires at time 3, group members have **write** access.

For the case on the picture above:
- At time 0. The subject would have **read** and **write** access to the object, because all three conditions described above are fulfilled for the groups: A and B.
- At time 1. The subject would have **read** access to the object, because membership of the subject in the group B has been expired.
- At time 2. The subject would have **no** access to the object, because the time of access to the object has been expired.


### How To Use

To build and start playing with the library, execute following shell commands:

```bash
## Building the development image and running the container with Riak KV within it.
$ ./run-docker.sh
## Building the application and executing an erlang shell.
$ make app shell
```

In the following example we will use `riakacl_rwaccess` ACL module
to demonstrate how you can manage access of subjects to objects.
Note that you can also create your own ACL modules by implementing `riakacl` behaviour.

```erlang
%% Initializing a connection to Riak KV.
{ok, S, _} = erl_scan:string(os:getenv("DEVELOP_ENVIRONMENT")),
{ok, Conf} = erl_parse:parse_term(S),
#{kv_protobuf := #{host := Host, port := Port}} = Conf,
{ok, Pid} = riakc_pb_socket:start_link(Host, Port).

%% Specifying buckets
SubjectBucket = {<<"riakacl_subject_t">>, <<"riakacl-object">>},
ObjectBucket = {<<"riakacl_object_t">>, <<"riakacl-object">>}.

%% Suppose we have "The Book" and want to restrict access.
%% Let's allow the group "a" to read it and the group "b" to write to it.
riakacl:put_object_acl(
  Pid, ObjectBucket, <<"The Book">>,
  [ {<<"reader">>, riakacl_rwaccess:new_dt(#{read => true})},
    {<<"writer">>, riakacl_rwaccess:new_dt(#{write => true})} ]).

%% Let "John" be a member of the "a" group. So that he have access with read permissions.
riakacl:put_subject_groups(
  Pid, SubjectBucket, <<"John">>,
  [ {<<"reader">>, riakacl_group:new_dt()} ]),
riakacl:authorize(Pid, SubjectBucket, <<"John">>, ObjectBucket, <<"The Book">>, riakacl_rwaccess).
%% {ok,#{read => true,write => false}}

%% Sinse "Jack" is a member of both groups, he have access with read and write permissions.
riakacl:put_subject_groups(
  Pid, SubjectBucket, <<"Jack">>,
  [ {<<"reader">>, riakacl_group:new_dt()},
    {<<"writer">>, riakacl_group:new_dt()} ]),
riakacl:authorize(Pid, SubjectBucket, <<"Jack">>, ObjectBucket, <<"The Book">>, riakacl_rwaccess).
%% {ok,#{read => true,write => true}}

%% Mery isn't a member of any group. Access to "The Book" is forbidden for her.
riakacl:put_subject_groups(Pid, SubjectBucket, <<"Mery">>, []),
riakacl:authorize(Pid, SubjectBucket, <<"Mery">>, ObjectBucket, <<"The Book">>, riakacl_rwaccess).
%% {ok,#{read => false,write => false}}

%% We can grant membership of group for limited time.
%% Note that unix time in microseconds is assumed to use with the library.
Time = riakacl:unix_time_us(), Before = Time -1, After = Time +1,
riakacl:put_subject_groups(
  Pid, SubjectBucket, <<"Mark">>,
  [ {<<"reader">>, riakacl_group:new_dt(#{exp => Time})} ]),
riakacl:authorize(Pid, SubjectBucket, <<"Mark">>, ObjectBucket, <<"The Book">>, [], riakacl_rwaccess, Before),
%% {ok,#{read => true,write => false}}
riakacl:authorize(Pid, SubjectBucket, <<"Mark">>, ObjectBucket, <<"The Book">>, [], riakacl_rwaccess, After).
%% {ok,#{read => false,write => false}}

%% In the same way we can expire ACL entry of the object itself.
riakacl:put_object_acl(
  Pid, ObjectBucket, <<"The Note">>,
  [ {<<"reader">>, riakacl_rwaccess:new_dt(#{read => true}, #{exp => Time})} ]),
riakacl:authorize(Pid, SubjectBucket, <<"John">>, ObjectBucket, <<"The Note">>, [], riakacl_rwaccess, Before),
%% {ok,#{read => true,write => false}}
riakacl:authorize(Pid, SubjectBucket, <<"John">>, ObjectBucket, <<"The Note">>, [], riakacl_rwaccess, After).
%% {ok,#{read => false,write => false}}

%% We can also specify predefined subject's groups or objects's ACL entries:
riakacl:authorize_predefined_subject(
  Pid, [<<"reader">>],
  ObjectBucket, <<"The Book">>,
  riakacl_rwaccess).
%% {ok,#{read => true,write => false}}
riakacl:authorize_predefined_subject(
  Pid, SubjectBucket, <<"John">>, [<<"nobody">>],
  ObjectBucket, <<"The Book">>,
  riakacl_rwaccess).
%% {ok,#{read => true,write => false}}
riakacl:authorize_predefined_object(
  Pid, SubjectBucket, <<"John">>,
  [{<<"reader">>, #{read => true, write => false}}],
  riakacl_rwaccess). 
%% {ok,#{read => true,write => false}}
riakacl:authorize_predefined_object(
  Pid, SubjectBucket, <<"John">>,
  ObjectBucket, <<"The Book">>, [{<<"nobody">>, #{read => false, write => false}}],
  riakacl_rwaccess).
%% {ok,#{read => true,write => false}}


%% We could also explicitly add group membership to a subject, just for one-time operation
riakacl:authorize_predefined_object(
  Pid, SubjectBucket, <<"Mery">>,
  ObjectBucket, <<"The Book">>, [],
  [<<"reader">>],
  riakacl_rwaccess).
%% {ok,#{read => true,write => false}}
riakacl:authorize_predefined_subject(
  Pid, SubjectBucket, <<"Mery">>, [],
  ObjectBucket, <<"The Book">>,
  [<<"reader">>],
  riakacl_rwaccess).
%% {ok,#{read => true,write => false}}


%% We can get membership back from a subject
riakacl:remove_subject_groups(Pid, SubjectBucket, <<"John">>, [<<"reader">>]).
%% or an object.
riakacl:remove_object_acl(Pid, ObjectBucket, <<"The Book">>, [<<"reader">>]).
%% In this way "John" won't access the "The Book" anymore.
riakacl:authorize(Pid, SubjectBucket, <<"John">>, ObjectBucket, <<"The Book">>, riakacl_rwaccess).

%% We get an error when trying to retrieve a non-existent ACL of subject
riakacl:authorize(Pid, SubjectBucket, <<"Foo">>, ObjectBucket, <<"The Book">>, riakacl_rwaccess).
%% {error,bad_aclsubject_key}
%% Or object
riakacl:authorize(Pid, SubjectBucket, <<"John">>, ObjectBucket, <<"Bar">>, riakacl_rwaccess).
%% {error,bad_aclobject_key}


%% Hint: you can always retrieve subject's or object's list of verified groups.
gb_sets:to_list(
  riakacl_entry:verified_groupset_dt(
    riakacl_entry:get(Pid, SubjectBucket, <<"Jack">>),
    riakacl:unix_time_us())).
```



### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[travis]:https://travis-ci.org/manifest/riak-acl?branch=master
[travis-img]:https://secure.travis-ci.org/manifest/riak-acl.png?branch=master
[riak-acl-groups-img]:misc/riak-acl-groups.png
