drop table link_queue;
drop table link;


create table link (
    id                      serial not null primary key,
    url                     varchar(255) not null,
    title                   varchar(255) not null,
    user_id                 int references snap_auth_user(uid) on delete cascade,
    submit_time             timestamp default(now())
);

create table link_queue (
    user_id                 bigint references snap_auth_user(uid) on delete cascade,
    link_id                 bigint references link(id) on delete cascade
);


