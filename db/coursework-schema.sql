create type exam_mark as enum (
  'passed', 'not passed'
);

create type personal_value as enum (
  'courage', 'bravery',
  'hard work', 'patience', 'justice', 'loyalty',
  'intelligence', 'creativity', 'wit',
  'ambition', 'cunning', 'leadership', 'resourcefulness'
);

create type gender as enum (
  'female', 'male', 'other'
);

create type creature_classification as enum (
  'x', 'xx', 'xxx', 'xxxx', 'xxxxx'
);

create type coordinates as (
  latitude  double precision,
  longitude double precision
);

create type delivery_content as enum (
  '1st class mail', '2nd class mail', 'parcel', 'periodical'
);

create type club_category as enum (
  'sport', 'art', 'science', 'other'
);

create type week_day as enum (
  'Sunday', 'Monday', 'Tuesday',
  'Wednesday', 'Thursday', 'Friday', 'Saturday'
);

create table houses (
  id      bigserial         not null
    constraint faculties_pkey
    primary key,
  name    text              not null,
  dean_id bigint            not null,
  values  personal_value [] not null
);

create unique index faculties_id_uindex
  on houses (id);

create table people (
  id             bigserial      not null
    constraint people_pkey
    primary key,
  full_name      text           not null,
  birth_date     timestamp      not null,
  bio            text,
  personal_value personal_value not null,
  gender         gender         not null,
  death_date     timestamp,

  constraint birth_death_date_order check (birth_date < death_date)
);

create unique index people_id_uindex
  on people (id);

alter table houses
  add constraint faculties_people_dean_id_fk
foreign key (dean_id) references people;

create table events
(
  id         bigserial not null
    constraint events_pkey
    primary key,
  name       text,
  ended_on   timestamp not null,
  started_on timestamp not null,

  constraint start_end_date_order check (started_on <= ended_on)
);

create unique index events_id_uindex
  on events (id);

create table event_participation
(
  id        bigserial not null
    constraint event_participation_pkey
    primary key,
  event_id  bigint    not null
    constraint event_participation_event_id_fk
    references events,
  person_id bigint    not null
    constraint event_participation_person_id_fk
    references people,
  positive  boolean   not null,
  score     integer   not null,
  date      timestamp not null,

  constraint positive_score check (score > 0)
);

create unique index event_participation_id_uindex
  on event_participation (id);

create table creatures
(
  generic_name       text                    not null,
  discovered_on      timestamp               not null,
  id                 bigserial               not null
    constraint creatures_id_pk
    primary key,
  mom_classification creature_classification not null
);

create unique index creatures_id_uindex
  on creatures (id);

create table books
(
  id                  bigserial not null
    constraint documents_pkey
    primary key,
  title               text      not null,
  added_on            timestamp not null,
  requires_permission boolean   not null
);

create unique index documents_id_uindex
  on books (id);

create table creature_books
(
  creature_id bigint not null
    constraint creature_documentation_creature_id_fk
    references creatures,
  book_id     bigint
    constraint creature_documentation_book_id_fk
    references books
);

create table student_profiles
(
  id             bigserial not null
    constraint student_profiles_pkey
    primary key,
  person_id      bigint    not null
    constraint student_profiles_person_id_fk
    references people,
  study_plan_id  bigint    not null,
  dormitory_room integer   not null
);

create unique index student_profiles_id_uindex
  on student_profiles (id);

create unique index student_profiles_person_id_uindex
  on student_profiles (person_id);

create table study_plans
(
  id            bigserial not null
    constraint study_plans_pkey
    primary key,
  house_id      bigint    not null
    constraint study_plans_house_id_fk
    references houses,
  academic_year integer   not null,

  constraint academic_year_validity check (academic_year between 1 and 7)
);

create unique index study_plans_id_uindex
  on study_plans (id);

alter table student_profiles
  add constraint student_profiles_study_plan_id_fk
foreign key (study_plan_id) references study_plans;

create table subjects
(
  id            bigserial not null
    constraint subjects_pkey
    primary key,
  name          text      not null,
  study_plan_id bigint    not null
    constraint subjects_study_plan_id_fk
    references study_plans,
  teacher_id    bigint    not null
    constraint subjects_people_teacher_id_fk
    references people
);

create unique index subjects_id_uindex
  on subjects (id);

create table exam_results
(
  id                 bigserial                                   not null
    constraint exam_results_id_pk
    primary key,
  subject_id         bigint                                      not null
    constraint exam_results_subject_id_fk
    references subjects,
  student_profile_id bigint
    constraint exam_results_student_profile_id_fk
    references student_profiles,
  mark               exam_mark default 'not passed' :: exam_mark not null
);

create unique index exam_results_id_uindex
  on exam_results (id);

create table book_lendings
(
  id              bigserial not null
    constraint book_lendings_pkey
    primary key,
  book_id         bigint    not null
    constraint book_lendings_book_id_fk
    references books,
  lendee_id       bigint    not null
    constraint book_lendings_lendee_id_fk
    references people,
  permitted_by_id bigint
    constraint book_lendings_permitted_by_id_fk
    references people,
  checked_out_on  timestamp not null,
  checked_in_on   timestamp,

  constraint checked_out_checked_in_date_order check (checked_out_on < book_lendings.checked_in_on)
);

create unique index book_lendings_id_uindex
  on book_lendings (id);

create table creature_domestications
(
  id                 bigserial not null
    constraint creature_domestications_pkey
    primary key,
  creature_id        bigint    not null
    constraint creature_domestications_creature_id_fk
    references creatures,
  domesticated_by_id bigint    not null
    constraint creature_domestications_domesticated_by_id_fk
    references people,
  domesticated_on    timestamp not null,
  name_given         text      not null
);

create unique index creature_domestications_id_uindex
  on creature_domestications (id);

create table delivery_owls (
  name     text not null,
  id       serial primary key,
  age      int  not null,
  house_id integer references houses (id)
);

create table delivery_owl_flights (
  id               serial,
  sender_id        integer references people (id),
  owl_id           integer references delivery_owls (id),
  dest_coordinates coordinates      not null,
  contents_type    delivery_content not null,
  departed_on      timestamp        not null,
  returned_on      timestamp
);

create table delivery_owl_repair_jobs (
  id                  serial,
  owl_id              integer references delivery_owls (id),
  tech_ops_maneger_id integer references people (id),
  cause               text      not null,
  began_on            timestamp not null,
  finished_on         timestamp
);

create table student_clubs (
  id           serial primary key,
  president_id integer references people (id),
  name         text            not null,
  category     club_categories not null
);

create table classroom_bookings (
  id                 serial primary key,
  subject_id         integer references subjects (id),
  student_profile_id integer references student_profiles (id),
  room_number        integer  not null,
  week_day           week_day not null,
  occupied_from      time     not null,
  occupied_to        time     not null
);

-- check_classroom_availability

create function check_classroom_availability()
  returns trigger as $$
declare
  is_occupied boolean;
begin
  is_occupied := (select exists(select
                                from classroom_bookings
                                where room_number = new.room_number
                                      and week_day = new.weekday,
                                and ((new.occupied_from >= occupied_from and new.occupied_from <= occupied_to)
                                     or (new.occupied_to >= occupied_from and new.occupied_to <= occupied_to))
  ));

  if is_occupied
  then raise exception 'The requested classroom is already booked for this time.';
  end if;
end;
$$
language plpgsql;

create trigger validate_classroom_availability
  before insert
  on classroom_bookings
  for each row execute procedure check_classroom_availability();

-- check_book_permission_before_checkout

create function check_book_permission_before_checkout()
  returns trigger as $$
declare
  requires_permission boolean;
begin
  requires_permission := (select requires_permission
                          from books
                          where id = new.book_id);
  if requires_permission and new.permitted_by_id is null
  then raise exception 'A permission is required to check out the requested book.';
  end if;
end;
$$
language plpgsql;

create trigger validate_book_checkout_policy
  before insert
  on book_lendings
  for each row execute procedure check_book_permission_before_checkout();

-- check_house_student_value_alignment

create function check_house_student_value_alignment()
  returns trigger as $$
declare
  house_values personal_value [];
begin
  house_values := (select values
                   from houses
                     inner join study_plans
                       on houses.id = study_plans.house_id
                     inner join study_plans
                       on study_plans.id = new.student_plan_id);
  if not (new.value = any (house_values))
  then raise exception 'Student''s personal value does not align with those of the house';
  end if;
end;
$$
language plpgsql;

create trigger validate_house_student_values
  before insert
  on student_profiles
  for each row execute procedure check_house_student_value_alignment();

-- check_owl_availability

create function check_owl_availability()
  returns trigger as $$
declare
  is_in_flight      boolean;
  is_being_repaired boolean;
begin
  is_in_flight := (select exists(select
                                 from delivery_owl_flights
                                 where owl_id = new.owl_id and returned_on is null));
  is_being_repaired := (select exists(select
                                      from delivery_owl_repair_jobs
                                      where owl_id = new.owl_id and finished_on is null));
  if is_in_flight
  then raise exception 'The requested owl is currently in flight';
  end if;
  if is_being_repaired
  then raise exception 'The requested owl is currently being repaired';
  end if;
end;
$$
language plpgsql;

create trigger validate_owl_availability
  before insert
  on delivery_owl_flights
  for each row execute procedure check_owl_availability();
