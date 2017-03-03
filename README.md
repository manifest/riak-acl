# Riak ACL

[![Build Status][travis-img]][travis]

Access control list (ACL) on top of Riak KV



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
%% Specifying buckets and opening a connection.
SubjectBucket = {<<"riakacl_subject_t">>, <<"riakacl-object">>},
ObjectBucket = {<<"riakacl_object_t">>, <<"riakacl-object">>},
{ok, Pid} = riakc_pb_socket:start_link("192.168.99.100", 8087).

%% Suppose we have "The Book" and want to restrict access.
%% Let's allow the group "a" to read it and the group "b" to write to it.
riakacl:put_object_acl(
  Pid, ObjectBucket, <<"The Book">>,
  [ {<<"a">>, riakacl_rwaccess:new_dt(#{read => true})},
    {<<"b">>, riakacl_rwaccess:new_dt(#{write => true})} ]).

%% Let "John" be a member of the "a" group. So that he have access with read permissions.
riakacl:put_subject_groups(
  Pid, SubjectBucket, <<"John">>,
  [ {<<"a">>, riakacl_group:new_dt()} ]),
riakacl:authorize(Pid, SubjectBucket, <<"John">>, ObjectBucket, <<"The Book">>, riakacl_rwaccess).
%% {ok,#{read => true,write => false}}

%% Sinse "Jack" is a member of both groups, he have access with read and write permissions.
riakacl:put_subject_groups(
  Pid, SubjectBucket, <<"Jack">>,
  [ {<<"a">>, riakacl_group:new_dt()},
    {<<"b">>, riakacl_group:new_dt()} ]),
riakacl:authorize(Pid, SubjectBucket, <<"Jack">>, ObjectBucket, <<"The Book">>, riakacl_rwaccess).
%% {ok,#{read => true,write => true}}

%% Mery isn't a member of any group. Access to "The Book" is forbidden for her.
riakacl:put_subject_groups(Pid, SubjectBucket, <<"Mery">>, []),
riakacl:authorize(Pid, SubjectBucket, <<"Mery">>, ObjectBucket, <<"The Book">>, riakacl_rwaccess).
%% error

%% We can grant membership of group for limited time.
%% Note that unix time in microseconds is assumed to use with the library.
Time = riakacl:unix_time_us(), Before = Time -1, After = Time +1,
riakacl:put_subject_groups(
  Pid, SubjectBucket, <<"Mark">>,
  [ {<<"a">>, riakacl_group:new_dt(#{exp => Time})} ]),
riakacl:authorize(Pid, SubjectBucket, <<"Mark">>, ObjectBucket, <<"The Book">>, riakacl_rwaccess, Before),
%% {ok,#{read => true,write => false}}
riakacl:authorize(Pid, SubjectBucket, <<"Mark">>, ObjectBucket, <<"The Book">>, riakacl_rwaccess, After).
%% error

%% In the same way we can expire ACL entry of the object itself.
riakacl:put_object_acl(
  Pid, ObjectBucket, <<"The Note">>,
  [ {<<"a">>, riakacl_rwaccess:new_dt(#{read => true}, #{exp => Time})} ]),
riakacl:authorize(Pid, SubjectBucket, <<"John">>, ObjectBucket, <<"The Note">>, riakacl_rwaccess, Before),
%% {ok,#{read => true,write => false}}
riakacl:authorize(Pid, SubjectBucket, <<"John">>, ObjectBucket, <<"The Note">>, riakacl_rwaccess, After).
%% error

%% We can get membership back from a subject
riakacl:remove_subject_groups(Pid, SubjectBucket, <<"John">>, [<<"a">>]).
%% or an object.
riakacl:remove_object_acl(Pid, ObjectBucket, <<"The Book">>, [<<"a">>]).
%% In this way "John" won't access the "The Book" anymore.
riakacl:authorize(Pid, SubjectBucket, <<"John">>, ObjectBucket, <<"The Book">>, riakacl_rwaccess).

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
[travis-img]:https://secure.travis-ci.org/manifest/riak-acl.png
