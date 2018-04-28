begin;
insert into people(full_name, birth_date, bio, personal_value, gender, death_date) values
  ('Minerva McGonagall', '1931-10-04', '', 'courage', 'female', null),
  ('Pomona Sprout', '1941-05-15', '', 'hard work', 'female', null),
  ('Filius Flitwick', '1958-10-17', '', 'wit', 'male', null),
  ('Severus Snape', '1960-01-06', '', 'leadership', 'male', null),
  ('Harry Potter', '1980-07-31', '', 'bravery', 'male', null),
  ('Hermiona Granger', '1979-09-19', '', 'courage', 'female', null),
  ('Albus Dumbledore', '1881-07-29', '', 'bravery', 'male', null);

insert into houses(name, dean_id, values) values
  ('Gryffindor', 1, '{"courage", "bravery"}'),
  ('Hufflepuff', 2, '{"hard work", "patience", "justice", "loyalty"}'),
  ('Ravenclaw', 3, '{"intelligence", "creativity", "wit"}'),
  ('Slytherin', 4, '{"ambition", "cunning", "leadership", "resourcefulness"}');

insert into books(title, added_on, requires_permission, author) values
  ('Ultimate Spell Encyclopedia', '1996-05-13', true, 'Merlin'),
  ('Perfume Of Maths', '1998-07-25', false, 'Kudryavceva'),
  ('Classified Files C7354', '1970-01-01', true, 'Nester Dalievich'),
  ('Harry Potter and Phylosopher`s Stone', '1995-08-15', false, 'J.R.Rowling'),
  ('Fantastic Creatures and Where to Find Them', '1999-05-26', false, 'J.K.Rowling');

insert into book_lendings(book_id, lendee_id, permitted_by_id, checked_out_on, checked_in_on) values
  (2, 6, null,'1994-10-19 10:23:54', '1994-11-19 12:10:32'),
  (5, 4, null,'1995-09-21 11:45:03', '1995-09-21 18:13:44'),
  (1, 6, 4,'1996-05-13 11:45:03', '1996-09-21 18:13:44' );

insert into creatures(generic_name, discovered_on, mom_classification) values
  ('Phoenix', '1556-05-23', 'xxxx'),
  ('Unicorn', '1639-07-15', 'xxxx'),
  ('Fire crab', '1702-03-24', 'xxx'),
  ('Winged horse', '1654-08-16', 'xx');

insert into creature_domestications(creature_id, domesticated_by_id, domesticated_on, name_given) values
  (1, 4, '1988-07-24 15:09:27', 'Infernal Isabel'),
  (3, 5, '1999-08-16 16:10:55', 'Ronald Jr.');

insert into creature_books(creature_id, book_id) values
  (1,5),
  (2,5),
  (3,5),
  (4,5);

insert into spells(counterspell_id, creator_id, name, description, type, is_forbidden) values
  (null, null, 'Avada Kedavra', 'Killing curse', 'curse', true),
  (null, null, 'Levicorpus', 'Levicorpus is the incantation of a jinx', 'curse', false),
  (2, null, 'Liberacorpus', 'Anti-Levicorpus', 'healing spell', false);

update spells set counterspell_id = 3 where id = 2;

insert into spell_books(spell_id, book_id) values
  (1,1),
  (2,1),
  (3,1);

insert into study_plans(house_id, academic_year) values
  (1,1),
  (2,1),
  (3,1),
  (4,1),
  (1,7),
  (2,7),
  (3,7),
  (4,7);

set constraints student_clubs_president_id_fkey deferred;

insert into student_clubs(name, president_id, category) values
  ('Doki-Doki Literature Club', 6, 'art'),
  ('K-on', 5, 'art'),
  ('Future Dean`s club', 4, 'other');

insert into student_profiles(person_id, club_id, study_plan_id, dormitory_room) values
  (1, 3, 1, 505),
  (2, 3, 2, 505),
  (3, 3, 3, 42),
  (4, 3, 4, 42),
  (5, 2, 1, 42),
  (6, 1, 1, 505);

insert into subjects(name, study_plan_id, teacher_id) values
  ('Transfiguration', 1, 1),
  ('Charms', 1, 3),
  ('Potions', 1, 4),
  ('Herbology', 1, 2);

insert into exam_results(subject_id, student_profile_id, attended_on, mark) values
  (1, 5, '1998-06-14 23:00:00', 'passed'),
  (2, 5, '1998-06-05 23:00:00', 'passed'),
  (3, 5, '1998-06-11 23:00:00', 'passed'),
  (4, 5, '1998-06-08 23:00:00', 'passed'),
  (1, 6, '1998-06-14 23:00:00', 'passed'),
  (2, 6, '1998-06-05 23:00:00', 'passed'),
  (3, 6, '1998-06-11 23:00:00', 'passed'),
  (4, 6, '1998-06-08 23:00:00', 'passed');

insert into classroom_bookings(subject_id, student_club_id, room_number, week_day, occupied_from, occupied_to) values
  (1, null, 505, 'Friday', '15:00', '16:30'),
  (null, 1, 42, 'Wednesday', '14:00', '15:30'),
  (2, null, 624, 'Monday', '10:00', '11:30'),
  (null, 2, 42, 'Friday', '15:00', '16:30');

insert into events(name, started_on, ended_on) values
  ('K-on Show', '1999-09-25 20:00:00', '1999-09-25 23:00:00'),
  ('Doki-Doki Poetry Reading', '1999-10-25 20:00:00', '1999-10-25 23:00:00');

insert into event_participations(event_id, student_profile_id, is_positive, score, date) values
  (1, 5, true, 10, '1999-09-25 21:23:00'),
  (2, 6, true, 10, '1999-10-25 22:47:00');

insert into delivery_owls(name, age, house_id) values
  ('Korizza', 6, 1),
  ('Cinnamon', 7, 1),
  ('Alta', 5, 1);

insert into delivery_owl_flights(sender_id, owl_id, dest_coordinates, contents_type, departed_on, returned_on) values
  (1, 1, row(15.74, 67.8753), '2nd class mail', '1995-05-25 16:35:42', '1995-05-26 18:27:18'),
  (5, 2, row(55.751999, 37.617734), '1st class mail', '1998-02-17 00:00:00', null),
  (6, 3, row(16.7379, 48.3745), 'parcel', '1998-10-21 16:38:41', '1998-10-23 12:20:43');

insert into delivery_owl_repair_jobs(owl_id, tech_ops_manager_id, cause, began_on, finished_on) values
  (1, 2, 'The owl has broken', '1997-01-15 17:16:43', '1997-02-15 19:35:10'),
  (2, 7, 'Scheduled maintenance', '1999-03-16 10:45:35', '1999-03-16 20:27:15'),
  (3, 1, 'The owl has broken', '1995-11-21 19:10:55', '1995-11-22 19:33:21');

commit;
