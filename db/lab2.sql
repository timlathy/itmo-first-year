create table crew_members (
  id serial primary key,
  full_name text not null,
  role text not null,
  age smallint not null constraint valid_age check (age >= 0)
);

create type interaction_kind as enum (
  'competition', 'conflict', 'cooperation'
);

create type interaction_side as enum (
  'initiating', 'responding'
);

create type interaction_subject as enum (
  'mission', 'relationship', 'game'
);

create table interactions (
  id serial primary key,
  kind interaction_kind not null,
  winning_side interaction_side,
  constraint valid_winning_side check (
    (kind = 'cooperation' and winning_side is null) or
    (kind != 'cooperation' and winning_side is not null)
  ),
  subject interaction_subject not null,
  details text,
  date date not null
);

create table interaction_participants (
  member_id integer references crew_members (id),
  interaction_id integer references interactions (id),
  constraint single_interaction_participation unique (member_id, interaction_id),
  side interaction_side not null
);

create type relationship_kind as enum (
  'friendship', 'romance', 'antagonism'
);

create table relationships (
  id serial primary key,
  confirming_interaction_id integer references interactions (id) not null,
  ending_interaction_id integer references interactions (id),
  kind relationship_kind not null,
  constraint separate_confirming_ending_interactions check (
    confirming_interaction_id != ending_interaction_id
  )
);

create table crew_members_relationships (
  member_id integer references crew_members (id) not null,
  relationship_id integer references relationships (id) not null,
  constraint single_relationship_participation unique (member_id, relationship_id)
);

create table ship_rooms (
  id serial primary key,
  name text not null,
  capacity integer not null
);

create table crew_relocations (
  id serial primary key,
  member_id integer references crew_members (id) not null,
  target_room_id integer references ship_rooms (id) not null,
  date date not null
);

create table observation_tools (
  id serial primary key,
  room_id integer references ship_rooms (id) not null,
  name text not null
);

create table scientific_observations (
  id serial primary key,
  observer_id integer references crew_members (id) not null,
  base_observation_id integer references scientific_observations (id),
  constraint distinct_base_observation check (id != base_observation_id),
  equipment_id integer references observation_tools (id) not null,
  details text,
  observed_on date not null
);

create type broadcast_kind as enum (
  'message', 'music'
);

create table intercom_broadcasts (
  id serial primary key,
  initiated_by_id integer references crew_members (id) not null,
  kind broadcast_kind not null,
  contents text not null,
  initiated_on date not null,
  duration interval not null
);

create function check_room_capacity_before_relocation()
  returns trigger as $$
    declare
      max_room_capacity integer;
      room_occupants_count integer;
    begin
      room_occupants_count := (select count(*) from (
        select distinct on(member_id) 1 from crew_relocations
        where target_room_id = NEW.target_room_id
        order by member_id, date desc) as current_room_occupants);

      max_room_capacity := (select capacity
        from ship_rooms where id = NEW.target_room_id);

      if (room_occupants_count >= max_room_capacity) then
        raise exception 'Room % has hit a capacity limit', NEW.target_room_id;
      end if;

      return NEW;
    end;
  $$ language plpgsql;

create trigger validate_room_capacity before insert on crew_relocations
  for each row execute procedure check_room_capacity_before_relocation();