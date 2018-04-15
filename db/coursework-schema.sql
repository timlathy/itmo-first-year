begin;
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

create table houses (
  id      bigserial primary key,
  name    text              not null,
  dean_id bigint            not null,
  values  personal_value [] not null
);

create table people (
  id             bigserial primary key,
  full_name      text           not null,
  birth_date     timestamp      not null,
  bio            text,
  personal_value personal_value not null,
  gender         gender         not null,
  death_date     timestamp,

  constraint birth_death_date_order check (birth_date < death_date)
);

alter table houses
  add foreign key (dean_id) references people;

create table creatures (
  id                 bigserial primary key,
  generic_name       text                    not null,
  discovered_on      timestamp               not null,
  mom_classification creature_classification not null
);

create table books (
  id                  bigserial primary key,
  title               text      not null,
  added_on            timestamp not null,
  requires_permission boolean   not null
);

create table creature_books (
  creature_id bigint not null references creatures,
  book_id     bigint not null references books
);

create table student_clubs (
  id           serial primary key,
  name         text            not null,
  president_id integer         not null,
  category     club_categories not null
);

create table student_profiles (
  id             bigserial primary key,
  person_id      bigint references people,
  club_id        bigint references student_clubs,
  study_plan_id  bigint  not null,
  dormitory_room integer not null
);

alter table student_clubs
  add foreign key (president_id) references student_profiles;

create table study_plans (
  id            bigserial primary key,
  house_id      bigint  not null references houses,
  academic_year integer not null,

  constraint academic_year_validity check (academic_year between 1 and 7)
);

alter table student_profiles
  add foreign key (study_plan_id) references study_plans;

create table subjects (
  id            bigserial primary key,
  name          text not null,
  study_plan_id bigint references study_plans,
  teacher_id    bigint references people
);

create table exam_results (
  id                 bigserial primary key,
  subject_id         bigint    not null references subjects,
  student_profile_id bigint    not null references student_profiles,
  mark               exam_mark not null default 'not passed' :: exam_mark
);

create table book_lendings (
  id              bigserial primary key,
  book_id         bigint    not null references books,
  lendee_id       bigint    not null references people,
  permitted_by_id bigint references people,
  checked_out_on  timestamp not null,
  checked_in_on   timestamp,

  constraint checked_out_checked_in_date_order check (checked_out_on < book_lendings.checked_in_on)
);

create table creature_domestications (
  id                 bigserial primary key,
  creature_id        bigint    not null references creatures,
  domesticated_by_id bigint    not null references people,
  domesticated_on    timestamp not null,
  name_given         text      not null
);

create table delivery_owls (
  name     text not null,
  id       serial primary key,
  age      int  not null,
  house_id integer references houses (id)
);

create table delivery_owl_flights (
  id               serial,
  sender_id        integer          not null references people,
  owl_id           integer          not null references delivery_owls,
  dest_coordinates coordinates      not null,
  contents_type    delivery_content not null,
  departed_on      timestamp        not null,
  returned_on      timestamp
);

create table delivery_owl_repair_jobs (
  id                  serial,
  owl_id              integer   not null references delivery_owls,
  tech_ops_manager_id integer   not null references people,
  cause               text      not null,
  began_on            timestamp not null,
  finished_on         timestamp
);

create table classroom_bookings (
  id              serial primary key,
  subject_id      integer  not null references subjects,
  student_club_id integer  not null references student_clubs,
  room_number     integer  not null,
  week_day        week_day not null,
  occupied_from   time     not null,
  occupied_to     time     not null,

  constraint booked_by_single_entity check
  (((subject_id is not null) :: integer + (student_club_id is not null) :: integer) = 1)
);

create table events (
  id         bigserial primary key,
  name       text,
  ended_on   timestamp not null,
  started_on timestamp not null,

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

create table spells (
  id              serial primary key,
  counterspell_id integer references spells,
  creator_id      integer references people,
  name            text       not null,
  description     text       not null,
  type            spell_type not null,
  is_forbidden    boolean
);

create table spell_books (
  spell_id integer not null references spells,
  book_id  integer not null references books
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
                                      and week_day = new.week_day
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

-- check_event_participation_date_belonging_to_event_timespan

create function check_event_participation_date_belonging_to_event_timespan()
  returns trigger as $$
declare
  event events;
begin
  event := (select *
            from events
            where events.id = new.event_id);
  if (new.date < event.started_on or new.date > event.ended_on)
  then raise exception 'Event participation date is not included in the event time span';
  end if;
end;
$$
language plpgsql;

create trigger validate_event_participation_date_belonging_to_event_timespan
  before insert
  on event_participations
  for each row execute procedure check_event_participation_date_belonging_to_event_timespan();

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

create trigger validate_book_permission_on_checkout
  before insert
  on book_lendings
  for each row execute procedure check_book_permission_before_checkout();

-- check_book_availability_before_checkout

create function check_book_availability_before_checkout()
  returns trigger as $$
declare is_unavailable boolean;
begin
  is_unavailable := (select exists(select
                                   from book_lendings
                                   where book_id = new.book_id and checked_in_on is null));
  if is_unavailable
  then raise exception 'The required book has already been checked out.';
  end if;
end;
$$
language plpgsql;

create trigger validate_book_availability_on_checkout
  before insert
  on book_lendings
  for each row execute procedure check_book_availability_before_checkout();

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

-- check_owl_sender_house_match

create function check_delivery_owl_sender_house_match()
  returns trigger as $$
declare
  house_id     integer;
  owl_house_id integer;
begin
  house_id := (select house_id
               from study_plans
                 inner join student_profiles
                   on study_plans.id = student_profiles.study_plan_id
               where student_profiles.person_id = new.sender_id);
  owl_house_id := (select house_id
                   from delivery_owls
                   where delivery_owls.id = new.owl_id);
  if house_id is not null and owl_house_id != house_id
  then raise exception 'The required owl does not belong to the sender''s house';
  end if;
end;
$$
language plpgsql;

create trigger validate_delivery_owl_sender_house_match
  before insert
  on delivery_owl_flights
  for each row execute procedure check_delivery_owl_sender_house_match();

-- set_permission_requirement_on_forbidden_spell_books

create function set_permission_requirement_on_forbidden_spell_books()
  returns trigger as $$
declare
  is_forbidden boolean;
begin
  is_forbidden := (select is_forbidden
                   from spells
                   where id = new.spell_id);
  if is_forbidden
  then update books
  set requires_permission = true
  where id = new.book_id;
  end if;
end;
$$
language plpgsql;

create trigger enforce_spell_book_permission_policy
  after insert
  on spell_books
  for each row execute procedure set_permission_requirement_on_forbidden_spell_books();

commit;