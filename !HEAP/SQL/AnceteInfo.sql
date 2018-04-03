CREATE TABLE `www0005_base`.`anceteinfo` (
  `id_man` INT NOT NULL,
  `childrens` VARCHAR(60) NULL,
  `pasport_code` VARCHAR(10) NULL,
  `district_1` VARCHAR(30) NULL,
  `district_2` VARCHAR(30) NULL,
  `id_language` INT NULL,
  `benefits` VARCHAR(50) NULL,
  `educ_type` VARCHAR(2) NULL,
  `kontract_startday` VARCHAR(23) NULL,
  `who_pays_kontract` INT NULL,
  `pastSport` VARCHAR(45) NULL,
  `presentSport` VARCHAR(45) NULL,
  `futureSport` VARCHAR(45) NULL,
  `student_mother` INT NULL,
  `student_father` INT NULL,
  PRIMARY KEY (`id_man`));