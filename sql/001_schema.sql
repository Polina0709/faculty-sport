-- Схема "Спорт на факультеті" (MySQL 8.0-safe)

SET NAMES utf8mb4;
SET time_zone = '+00:00';
SET sql_notes = 0;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS enrollments;
DROP TABLE IF EXISTS schedule;
DROP TABLE IF EXISTS competitions;
DROP TABLE IF EXISTS coaches;
DROP TABLE IF EXISTS locations;
DROP TABLE IF EXISTS sections;
DROP TABLE IF EXISTS students;
DROP TABLE IF EXISTS teachers;
SET FOREIGN_KEY_CHECKS = 1;

-- STUDENTS
CREATE TABLE students (
                          id          INT AUTO_INCREMENT PRIMARY KEY,
                          first_name  VARCHAR(100) NOT NULL,
                          last_name   VARCHAR(100) NOT NULL,
                          group_code  VARCHAR(20)  NOT NULL,
                          birthdate   DATE         NOT NULL,
                          email       VARCHAR(190) NOT NULL,
                          created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                          updated_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                          CONSTRAINT uq_students_email UNIQUE(email)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_students_group ON students(group_code);
CREATE INDEX idx_students_last_first ON students(last_name, first_name);

-- TEACHERS
CREATE TABLE teachers (
                          id         INT AUTO_INCREMENT PRIMARY KEY,
                          first_name VARCHAR(100) NOT NULL,
                          last_name  VARCHAR(100) NOT NULL,
                          position   VARCHAR(100) NOT NULL,
                          email      VARCHAR(190) NOT NULL,
                          created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                          updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                          CONSTRAINT uq_teachers_email UNIQUE(email)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_teachers_last_first ON teachers(last_name, first_name);

-- SECTIONS
CREATE TABLE sections (
                          id          INT AUTO_INCREMENT PRIMARY KEY,
                          name        VARCHAR(120) NOT NULL,
                          description TEXT         NOT NULL,
                          created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                          updated_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                          CONSTRAINT uq_sections_name UNIQUE(name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- LOCATIONS
CREATE TABLE locations (
                           id         INT AUTO_INCREMENT PRIMARY KEY,
                           name       VARCHAR(120) NOT NULL,
                           address    VARCHAR(255) NOT NULL,
                           created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                           updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                           CONSTRAINT uq_locations_name UNIQUE(name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- COACHES
CREATE TABLE coaches (
                         id          INT AUTO_INCREMENT PRIMARY KEY,
                         teacher_id  INT NOT NULL,
                         section_id  INT NOT NULL,
                         assigned_at DATE NOT NULL,
                         created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                         updated_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                         CONSTRAINT fk_coaches_teacher FOREIGN KEY (teacher_id) REFERENCES teachers(id) ON DELETE CASCADE ON UPDATE CASCADE,
                         CONSTRAINT fk_coaches_section FOREIGN KEY (section_id) REFERENCES sections(id) ON DELETE CASCADE ON UPDATE CASCADE,
                         CONSTRAINT uq_coach UNIQUE(teacher_id, section_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_coaches_section ON coaches(section_id);

-- SCHEDULE
CREATE TABLE schedule (
                          id           INT AUTO_INCREMENT PRIMARY KEY,
                          section_id   INT NOT NULL,
                          weekday      TINYINT NOT NULL,
                          start_time   TIME NOT NULL,
                          end_time     TIME NOT NULL,
                          location_id  INT NOT NULL,
                          created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                          updated_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                          CONSTRAINT fk_sched_section  FOREIGN KEY (section_id) REFERENCES sections(id)  ON DELETE CASCADE ON UPDATE CASCADE,
                          CONSTRAINT fk_sched_location FOREIGN KEY (location_id) REFERENCES locations(id) ON DELETE RESTRICT ON UPDATE CASCADE,
                          CONSTRAINT chk_weekday CHECK (weekday BETWEEN 1 AND 7),
                          CONSTRAINT chk_time_range CHECK (start_time < end_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_schedule_section ON schedule(section_id, weekday, start_time);

-- COMPETITIONS
CREATE TABLE competitions (
                              id           INT AUTO_INCREMENT PRIMARY KEY,
                              title        VARCHAR(200) NOT NULL,
                              section_id   INT NOT NULL,
                              date_day     DATE NOT NULL,
                              location_id  INT NOT NULL,
                              created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                              updated_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                              CONSTRAINT fk_comp_section  FOREIGN KEY (section_id) REFERENCES sections(id)  ON DELETE CASCADE ON UPDATE CASCADE,
                              CONSTRAINT fk_comp_location FOREIGN KEY (location_id) REFERENCES locations(id) ON DELETE RESTRICT ON UPDATE CASCADE,
                              CONSTRAINT chk_comp_range CHECK (date_day >= '1900-01-01')
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_comp_date ON competitions(date_day);
CREATE INDEX idx_comp_section ON competitions(section_id);

-- ENROLLMENTS
CREATE TABLE enrollments (
                             id           INT AUTO_INCREMENT PRIMARY KEY,
                             student_id   INT NOT NULL,
                             section_id   INT NOT NULL,
                             enrolled_at  DATE NOT NULL,
                             created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                             updated_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                             CONSTRAINT fk_enr_student FOREIGN KEY (student_id) REFERENCES students(id) ON DELETE CASCADE ON UPDATE CASCADE,
                             CONSTRAINT fk_enr_section FOREIGN KEY (section_id) REFERENCES sections(id) ON DELETE CASCADE ON UPDATE CASCADE,
                             CONSTRAINT uq_enrollment UNIQUE(student_id, section_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE INDEX idx_enrollments_student ON enrollments(student_id);
CREATE INDEX idx_enrollments_section ON enrollments(section_id);

SET sql_notes = 1;
