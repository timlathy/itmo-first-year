-- check_classroom_availability

create function check_classroom_availability()
  returns trigger as $$
declare
  is_occupied boolean;
begin
  is_occupied := (select exists(
      select from classroom_bookings
      where room_number = new.room_number and week_day = new.week_day
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
  before insert on classroom_bookings for each row execute procedure check_classroom_availability();

-- check_event_participation_date_belonging_to_event_timespan

create function check_event_participation_date_belonging_to_event_timespan()
  returns trigger as $$
declare
  event events;
begin
  event := (select * from events where events.id = new.event_id);

  if (new.date < event.started_on or new.date > event.ended_on)
  then raise exception 'Event participation date is not included in the event time span';
  end if;
end;
$$
language plpgsql;

create trigger validate_event_participation_date_belonging_to_event_timespan
  before insert on event_participations
  for each row execute procedure check_event_participation_date_belonging_to_event_timespan();

-- check_book_permission_before_checkout

create function check_book_permission_before_checkout()
  returns trigger as $$
declare
  requires_permission boolean;
begin
  requires_permission := (select requires_permission from books where id = new.book_id);

  if requires_permission and new.permitted_by_id is null
  then raise exception 'A permission is required to check out the requested book.';
  end if;
end;
$$
language plpgsql;

create trigger validate_book_permission_on_checkout
  before insert on book_lendings for each row execute procedure check_book_permission_before_checkout();

-- check_book_availability_before_checkout

create function check_book_availability_before_checkout()
  returns trigger as $$
declare
  is_unavailable boolean;
begin
  is_unavailable := (select exists(
      select from book_lendings where book_id = new.book_id and checked_in_on is null));

  if is_unavailable
  then raise exception 'The required book has already been checked out.';
  end if;
end;
$$
language plpgsql;

create trigger validate_book_availability_on_checkout
  before insert on book_lendings for each row execute procedure check_book_availability_before_checkout();

-- check_house_student_value_alignment

create function check_house_student_value_alignment()
  returns trigger as $$
declare
  house_values personal_value [];
begin
  house_values := (select values from houses
     inner join study_plans on houses.id = study_plans.house_id
     inner join study_plans on study_plans.id = new.student_plan_id);

  if not (new.value = any (house_values))
  then raise exception 'Student''s personal value does not align with those of the house';
  end if;
end;
$$
language plpgsql;

create trigger validate_house_student_values
  before insert on student_profiles for each row execute procedure check_house_student_value_alignment();

-- check_owl_availability

create function check_owl_availability()
  returns trigger as $$
declare
  is_in_flight      boolean;
  is_being_repaired boolean;
begin
  is_in_flight := (select exists(
      select from delivery_owl_flights where owl_id = new.owl_id and returned_on is null));
  is_being_repaired := (select exists(
      select from delivery_owl_repair_jobs where owl_id = new.owl_id and finished_on is null));

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
  before insert on delivery_owl_flights for each row execute procedure check_owl_availability();

-- check_owl_sender_house_match

create function check_delivery_owl_sender_house_match()
  returns trigger as $$
declare
  house_id     integer;
  owl_house_id integer;
begin
  house_id := (select house_id from study_plans
    inner join student_profiles on study_plans.id = student_profiles.study_plan_id
    where student_profiles.person_id = new.sender_id);
  owl_house_id := (select house_id from delivery_owls where delivery_owls.id = new.owl_id);

  if house_id is not null and owl_house_id != house_id
  then raise exception 'The required owl does not belong to the sender''s house';
  end if;
end;
$$
language plpgsql;

create trigger validate_delivery_owl_sender_house_match
  before insert on delivery_owl_flights for each row execute procedure check_delivery_owl_sender_house_match();

-- set_permission_requirement_on_forbidden_spell_books

create function set_permission_requirement_on_forbidden_spell_books()
  returns trigger as $$
declare
  is_forbidden boolean;
begin
  is_forbidden := (select is_forbidden from spells where id = new.spell_id);

  if is_forbidden
  then update books set requires_permission = true where id = new.book_id;
  end if;
end;
$$
language plpgsql;

create trigger enforce_spell_book_permission_policy
  after insert on spell_books for each row execute procedure set_permission_requirement_on_forbidden_spell_books();
