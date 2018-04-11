CREATE TABLE `www0005_base`.`event` (
  `id_Event` INT NOT NULL AUTO_INCREMENT,
  `id_EventInfo` INT NOT NULL,
  `isGroup_Event` VARCHAR(5) NOT NULL,
  `fk_student_or_group` INT NOT NULL,
  `event_result` VARCHAR(250) NULL,
  `creatingDate` VARCHAR(23) NULL,
  PRIMARY KEY (`id_Event`),
  INDEX `id_EventInfo_idx` (`id_EventInfo` ASC),
  CONSTRAINT `id_EventInfo`
    FOREIGN KEY (`id_EventInfo`)
    REFERENCES `www0005_base`.`eventinfo` (`id_EventInfo`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION);
