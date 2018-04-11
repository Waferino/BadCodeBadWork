CREATE TABLE `www0005_base`.`eventinfo` (
  `id_EventInfo` INT NOT NULL AUTO_INCREMENT,
  `DateOfThe` DATETIME NULL,
  `whosEvent` VARCHAR(10) NOT NULL,
  `Name` VARCHAR(150) NOT NULL,
  `Notation` LONGTEXT NULL,
  `ChangedBy` INT NOT NULL,
  `creatingDate` VARCHAR(23) NULL,
  PRIMARY KEY (`id_EventInfo`))
COMMENT = 'Dictionary for Events names and dates';