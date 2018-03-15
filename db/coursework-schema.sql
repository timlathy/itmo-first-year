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

create table houses
(
  id      bigserial         not null
    constraint faculties_pkey
    primary key,
  name    text              not null,
  dean_id bigint            not null,
  values  personal_value [] not null
);

create unique index faculties_id_uindex
  on houses (id);

create table people
(
  id             bigserial      not null
    constraint people_pkey
    primary key,
  full_name      text           not null,
  birth_date     date           not null,
  bio            text,
  personal_value personal_value not null,
  gender         gender         not null,
  death_date     date
);

create unique index people_id_uindex
  on people (id);

alter table houses
  add constraint faculties_people_dean_id_fk
foreign key (dean_id) references people;

create table events
(
  id         serial not null
    constraint events_pkey
    primary key,
  name       text,
  ended_on   date   not null,
  started_on date   not null
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
  date      date      not null
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
  academic_year integer   not null
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
  checked_out_on  date      not null,
  checked_in_out  date
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
  domesticated_on    date      not null,
  name_given         text      not null
);

create unique index creature_domestications_id_uindex
  on creature_domestications (id);

