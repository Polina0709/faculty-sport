USE faculty_sport;

SET NAMES utf8mb4;
SET time_zone = '+00:00';

-- TEACHERS
INSERT INTO teachers(first_name, last_name, position, email) VALUES
                                                                 ('Олена','Сидоренко','Старший викладач','olena.sydorenko@univ.edu'),
                                                                 ('Павло','Іванченко','Доцент','pavlo.ivanchenko@univ.edu');

-- SECTIONS
INSERT INTO sections(name, description) VALUES
                                            ('Волейбол','Секція волейболу: тренування двічі на тиждень'),
                                            ('Баскетбол','Секція баскетболу: базові та просунуті групи'),
                                            ('Легка атлетика','Біг, стрибки, метання');

-- LOCATIONS
INSERT INTO locations(name, address) VALUES
                                         ('Спортзал №1','Корпус А, вул. Університетська, 1'),
                                         ('Спортзал №2','Корпус Б, вул. Університетська, 3'),
                                         ('Стадіон','вул. Спортивна, 10');

-- COACHES
INSERT INTO coaches(teacher_id, section_id, assigned_at) VALUES
                                                             (1, 1, CURRENT_DATE),
                                                             (2, 2, CURRENT_DATE),
                                                             (2, 3, CURRENT_DATE);

-- STUDENTS
INSERT INTO students(first_name, last_name, group_code, birthdate, email) VALUES
                                                                              ('Іван','Петренко','КН-11','2004-05-12','ivan.petrenko@univ.edu'),
                                                                              ('Марія','Коваленко','КН-12','2005-03-03','maria.kovalenko@univ.edu'),
                                                                              ('Сергій','Мельник','КН-11','2004-09-20','sergiy.melnyk@univ.edu'),
                                                                              ('Анна','Романюк','КН-21','2003-12-30','anna.romanyuk@univ.edu');

-- SCHEDULE
INSERT INTO schedule(section_id, weekday, start_time, end_time, location_id) VALUES
                                                                                 (1, 2, '18:00:00', '19:30:00', 1), -- Волейбол, вівторок
                                                                                 (1, 4, '18:00:00', '19:30:00', 1), -- Волейбол, четвер
                                                                                 (2, 3, '17:00:00', '18:30:00', 2), -- Баскетбол, середа
                                                                                 (2, 5, '17:00:00', '18:30:00', 2), -- Баскетбол, п’ятниця
                                                                                 (3, 6, '10:00:00', '12:00:00', 3); -- Легка атлетика, субота

-- ENROLLMENTS (записи студентів у секції)
INSERT INTO enrollments(student_id, section_id, enrolled_at) VALUES
                                                                 (1, 1, CURRENT_DATE),
                                                                 (2, 2, CURRENT_DATE),
                                                                 (3, 1, CURRENT_DATE),
                                                                 (4, 3, CURRENT_DATE);

-- COMPETITIONS
INSERT INTO competitions(title, section_id, date_day, location_id) VALUES
                                                                       ('Кубок факультету з волейболу', 1, DATE_ADD(CURRENT_DATE, INTERVAL 20 DAY), 1),
                                                                       ('Матч баскетболу “Студенти vs Викладачі”', 2, DATE_ADD(CURRENT_DATE, INTERVAL 30 DAY), 2),
                                                                       ('Легкоатлетичний крос', 3, DATE_ADD(CURRENT_DATE, INTERVAL 45 DAY), 3);
