create table users (
  id int primary key,
  name varchar(255),
  some_bool boolean,
  some_int64 bigint,
  some_int32 integer,
  some_float double precision,
  pets text[],
  pet_name varchar(255)
);

INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (1, 'Alice,', TRUE, null, 1, 2, 1.1, '{Lucy, Adam, "helllo,world"}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (2, 'Bob', TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (3, 'Charlie',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (4, 'David',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (5, 'Eve',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (6, 'Frank',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (7, 'Grace',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (8, 'Hannah',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (9, 'Ian',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (10, 'Jane',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (11, 'Kyle',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (12, 'Laura',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (13, 'Mike',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (14, 'Nora',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (15, 'Oscar',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (16, 'Patty',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (17, 'Quinn',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (18, 'Rachel',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (19, 'Steve',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (20, 'Tina',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (21, 'Ursula',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (22, 'Victor',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (23, 'Wendy',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (24, 'Xander',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (25, 'Yvonne',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (26, 'Zach',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (27, 'Amelia',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (28, 'Brad',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (29, 'Catherine',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (30, 'Derek',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (31, 'Elaine',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (32, 'Felix',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (33, 'Gloria',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (34, 'Harold',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (35, 'Iris',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (36, 'Justin',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (37, 'Kara',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (38, 'Leon',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (39, 'Mona',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (40, 'Ned',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (41, 'Olivia',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (42, 'Pete',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (43, 'Quincy',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (44, 'Rita',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (45, 'Sam',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (46, 'Tiffany',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (47, 'Ulysses',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (48, 'Vanessa',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (49, 'William',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (50, 'Xena',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (51, 'Yosef',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (52, 'Zoe',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (53, 'Adam',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (54, 'Betty',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (55, 'Carl',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (56, 'Diana',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (57, 'Ethan',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (58, 'Fiona',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (59, 'George',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (60, 'Heather',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (61, 'Ivan',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (62, 'Julia',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (63, 'Kevin',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (64, 'Linda',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (65, 'Marvin',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (66, 'Nancy',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (67, 'Oliver',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (68, 'Paula',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (69, 'Quentin',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (70, 'Rebecca',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (71, 'Sean',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (72, 'Tracy',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (73, 'Umar',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (74, 'Violet',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (75, 'Wesley',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (76, 'Xiomara',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (77, 'Yanni',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (78, 'Zelda',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (79, 'Aaron',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (80, 'Brenda',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (81, 'Cody',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (82, 'Debbie',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (83, 'Ernest',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (84, 'Felicia',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (85, 'Gary',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (86, 'Holly',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (87, 'Isaac',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (88, 'Jenna',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (89, 'Kenny',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (90, 'Lori',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (91, 'Manny',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (92, 'Nina',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (93, 'Owen',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (94, 'Pam',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (95, 'Quinton',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (96, 'Rosalind',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (97, 'Shane',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (98, 'Theresa',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (99, 'Uriah',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
INSERT INTO users (id, name, some_bool, pet_name, some_int64, some_int32, some_float, pets) VALUES (100, 'Veronica',TRUE,  'Danza', 1, 2, 1.1, '{Lucy, Adam}');
