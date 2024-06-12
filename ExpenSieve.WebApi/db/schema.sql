create table users (
    id serial primary key,
    email text not null unique,
    password text not null,
    first_name text not null,
    last_name text not null
);

create table transaction_groups (
    id serial primary key,
    user_id int not null,
    start_date timestamp not null,
    end_date timestamp not null,
    name text not null,

    foreign key (user_id) references users (id)
);

create table transactions (
    id serial primary key,
    group_id int not null,
    description text not null,
    amount decimal not null,
    kind text not null,

    foreign key (group_id) references transaction_groups (id)
);

create table fixed_transactions (
    id serial primary key,
    group_id int not null,
    description text not null,
    amount decimal not null,
    kind text not null,

    foreign key (group_id) references transaction_groups (id)
);
