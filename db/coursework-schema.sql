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

create type club_categories as enum (
  'sport', 'art', 'science', 'other'
);

create type week_day as enum (
  'Sunday', 'Monday', 'Tuesday',
  'Wednesday', 'Thursday', 'Friday', 'Saturday'
);

create type spell_type as enum (
  'conjuration', 'charm',
  'healing spell', 'curse', 'transfiguration'
);

create table people (
  id             bigserial primary key,
  full_name      text           not null,
  birth_date     date           not null,
  death_date     date,
  personal_value personal_value not null,
  gender         gender         not null,
  bio            text,

  constraint birth_death_date_order
  check (birth_date < death_date)
);

create table houses (
  id      bigserial primary key,
  name    text              not null,
  dean_id bigint            not null
    references people,
  values  personal_value [] not null
);

-- Creatures, spells, books

create table books (
  id                  bigserial primary key,
  title               text    not null,
  author              text    not null,
  added_on            date    not null,
  requires_permission boolean not null
);

create table book_lendings (
  id              bigserial primary key,
  book_id         bigint    not null references books,
  lendee_id       bigint    not null references people,
  permitted_by_id bigint references people,
  checked_out_on  timestamp not null,
  checked_in_on   timestamp,

  constraint checked_out_checked_in_date_order
  check (checked_out_on < book_lendings.checked_in_on)
);

create table creatures (
  id                 bigserial primary key,
  generic_name       text                    not null,
  discovered_on      timestamp               not null,
  mom_classification creature_classification not null
);

create table creature_domestications (
  id                 bigserial primary key,
  creature_id        bigint    not null references creatures,
  domesticated_by_id bigint    not null references people,
  domesticated_on    timestamp not null,
  name_given         text      not null
);

create table creature_books (
  creature_id bigint not null references creatures,
  book_id     bigint not null references books
);

create table spells (
  id              bigserial primary key,
  counterspell_id bigint references spells,
  creator_id      bigint references people,
  name            text       not null,
  description     text       not null,
  type            spell_type not null,
  is_forbidden    boolean    not null
);

create table spell_books (
  spell_id bigint not null references spells,
  book_id  bigint not null references books
);

-- Student activity

create table study_plans (
  id            bigserial primary key,
  house_id      bigint  not null references houses,
  academic_year integer not null,

  constraint academic_year_validity
  check (academic_year between 1 and 7)
);

create table student_clubs (
  id           bigserial primary key,
  name         text            not null,
  president_id bigint          not null,
  category     club_categories not null
);

create table student_profiles (
  id                  bigserial primary key,
  person_id           bigint not null references people,
  club_id             bigint not null references student_clubs,
  study_plan_id       bigint references study_plans,
  graduation_essay_id bigint references books,
  dormitory_room      integer,

  constraint graduation_removes_study_plan check
  (((study_plan_id is not null) :: integer
    + (graduation_essay_id is not null) :: integer) = 1)
);

alter table student_clubs
  add foreign key (president_id) references student_profiles
  deferrable initially immediate;

create table subjects (
  id            bigserial primary key,
  name          text not null,
  study_plan_id bigint references study_plans,
  teacher_id    bigint references people
);

create table exam_results (
  id                 bigserial primary key,
  subject_id         bigint    not null
    references subjects,
  student_profile_id bigint    not null
    references student_profiles,
  attended_on        timestamp not null,
  mark               exam_mark not null
    default 'not passed' :: exam_mark
);

-- Classroom arrangement

create table classroom_bookings (
  id              bigserial primary key,
  subject_id      bigint references subjects,
  student_club_id bigint references student_clubs,
  room_number     integer  not null,
  week_day        week_day not null,
  occupied_from   time     not null,
  occupied_to     time     not null,

  constraint booked_by_single_entity check
  (((subject_id is not null) :: integer
    + (student_club_id is not null) :: integer) = 1)
);

-- Events

create table events (
  id         bigserial primary key,
  name       text,
  started_on timestamp not null,
  ended_on   timestamp not null,

  constraint start_end_date_order check (started_on <= ended_on)
);

create table event_participations (
  id                 bigserial primary key,
  event_id           bigint    not null references events,
  student_profile_id bigint    not null references student_profiles,
  is_positive        boolean   not null,
  score              integer   not null,
  date               timestamp not null,

  constraint positive_score check (score > 0)
);

-- Delivery owls

create table delivery_owls (
  id       bigserial primary key,
  name     text    not null,
  age      integer not null,
  house_id bigint  not null
    references houses (id)
);

create table delivery_owl_flights (
  id               bigserial,
  sender_id        bigint           not null
    references people,
  owl_id           bigint           not null
    references delivery_owls,
  dest_coordinates coordinates      not null,
  contents_type    delivery_content not null,
  departed_on      timestamp        not null,
  returned_on      timestamp,

  constraint departure_return_date_order
  check (departed_on <= returned_on)
);

create table delivery_owl_repair_jobs (
  id                  bigserial,
  owl_id              bigint    not null
    references delivery_owls,
  tech_ops_manager_id bigint    not null
    references people,
  cause               text      not null,
  began_on            timestamp not null,
  finished_on         timestamp,

  constraint beginning_finish_date_order
  check (began_on <= finished_on)
);
