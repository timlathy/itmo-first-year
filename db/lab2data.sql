insert into crew_members (full_name, role, age) values
  ('Max Brailovsky', 'assistant engineer', 29),
  ('Zenia Marchenko', 'astronaut', 26),
  ('Heywood Floyd', 'scientific specialist', 52),
  ('Katerina Rudenko', 'chief medical officer', 34),
  ('Walter Curnow', 'engineer', 42),
  ('R. Chandra', 'scientist', 40);

insert into intercom_broadcasts
    (initiated_by_id, kind, contents, initiated_on, duration) values
  (3, 'message', 'Everyone please assemble in the observation room',
      '2010-01-16 15:35:09', '5 seconds'),
  (1, 'music', 'Relaxing space music',
      '2010-01-16 15:43:10', '3 hours');

insert into interactions (kind, winning_side, subject, details, date) values
  ('cooperation', null, 'mission',
   'A discussion concerning the details of the mission', '2010-01-12 12:23:23'),
  ('cooperation', null, 'relationship',
   'A nice, drawn-out conversation about everything and nothing in particular',
   '2010-01-14 12:23:23'),
  ('competition', 'initiating', 'game',
   'A friendly game of Chess', '2010-01-16 15:58:32'),
  ('competition', 'responding', 'game',
   'A friendly game of Chess', '2010-01-16 16:02:32');

insert into interaction_participants values
  (3, 1, 'initiating'), (5, 1, 'responding'),
  (6, 1, 'responding'), -- mission discussion
  (2, 2, 'initiating'), (1, 2, 'responding'), -- intimate conversation
  (1, 3, 'initiating'), (2, 3, 'responding'), -- game of chess
  (3, 4, 'initiating'), (5, 4, 'responding'); -- another game of chess

insert into relationships
    (confirming_interaction_id, ending_interaction_id, kind) values
  (1, null, 'friendship'), (2, null, 'romance');

insert into crew_members_relationships values
  (3, 1), (5, 1), (6, 1), -- friendship
  (1, 2), (2, 2); -- romance

insert into ship_rooms (name, capacity) values
  ('Crew Quarters', 6),
  ('Observation Room', 6),
  ('Control Room', 2),
  ('Power Room', 1),
  ('Storage Room', 1);

insert into observation_tools (room_id, name) values
  (2, 'Telescope'),
  (2, 'Portholes'),
  (1, 'Portable Chemistry Lab'),
  (3, 'Gamma-ray Spectrometer');

insert into scientific_observations
    (observer_id, base_observation_id, equipment_id, details, observed_on) values
  (3, null, 2, 'The ship is slowly approaching the target galaxy',
   '2010-01-16 16:12:20'),
  (3, 1, 1,
   'The velocity reported by the on-board computer appears' ||
   'to be inconsistent with manual observations', '2010-01-16 16:18:20'),
  (6, null, 2, 'The ship seems to be following the correct course',
   '2010-01-16 16:18:20');

insert into crew_relocations (member_id, target_room_id, date) values
  (3, 2, '2010-01-16 15:36:29'),
  (5, 2, '2010-01-16 15:36:29'),
  (6, 2, '2010-01-16 15:36:29'),
  (4, 2, '2010-01-16 15:37:21'),
  (1, 1, '2010-01-16 16:06:25'),
  (2, 1, '2010-01-16 16:06:25');