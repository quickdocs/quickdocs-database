CREATE TABLE IF NOT EXISTS project (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  ql_dist_version CHAR(10) NOT NULL,
  name VARCHAR(64) NOT NULL,
  release_version CHAR(10) NOT NULL,
  homepage_url TINYTEXT,
  repos_url TINYTEXT,
  archive_url TINYTEXT NOT NULL,
  PRIMARY KEY (id),
  UNIQUE KEY (ql_dist_version, name),
  KEY (name)
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS project_readme (
  project_id BIGINT UNSIGNED NOT NULL,
  filename TINYTEXT,
  raw TEXT,
  converted TEXT,
  FOREIGN KEY (project_id) REFERENCES project (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS system (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  project_id BIGINT UNSIGNED NOT NULL,
  name VARCHAR(64) NOT NULL,
  version VARCHAR(32),
  description TEXT,
  long_description TEXT,
  homepage_url TINYTEXT,
  license TEXT,
  PRIMARY KEY (id),
  UNIQUE KEY (project_id, name),
  FOREIGN KEY (project_id) REFERENCES project (id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS system_author (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  system_id BIGINT UNSIGNED NOT NULL,
  author_name VARCHAR(256) NOT NULL,
  type ENUM('author', 'maintainer') NOT NULL DEFAULT 'author',
  PRIMARY KEY (id),
  FOREIGN KEY (system_id) REFERENCES system (id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS system_dependencies (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  system_id BIGINT UNSIGNED NOT NULL,
  depends_system_id BIGINT UNSIGNED NOT NULL,
  is_for_defsystem TINYINT NOT NULL DEFAULT '0',
  PRIMARY KEY (id),
  UNIQUE KEY (system_id, depends_system_id, is_for_defsystem),
  FOREIGN KEY (system_id) REFERENCES system (id) ON DELETE CASCADE,
  FOREIGN KEY (depends_system_id) REFERENCES system (id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS system_packages (
  system_id BIGINT UNSIGNED NOT NULL,
  packages TEXT NOT NULL DEFAULT '',
  failed TINYINT NOT NULL DEFAULT '0',
  error_log TEXT NOT NULL DEFAULT '',
  FOREIGN KEY (system_id) REFERENCES system (id) ON DELETE CASCADE,
  UNIQUE KEY (system_id)
);

CREATE TABLE IF NOT EXISTS cliki (
  project_name VARCHAR(64) NOT NULL,
  body TEXT NOT NULL,
  updated_at INTEGER NOT NULL,
  UNIQUE KEY (project_name)
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS cliki_project_category (
  project_name VARCHAR(64) NOT NULL,
  category VARCHAR(256) NOT NULL,
  UNIQUE KEY (project_name, category),
  KEY (category)
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS repos_info (
  project_name VARCHAR(64) NOT NULL,
  type ENUM('github', 'bitbucket') NOT NULL,
  repos_id TINYTEXT NOT NULL,
  description TEXT,
  homepage_url TINYTEXT,
  watch_count INTEGER NOT NULL,
  forks_count INTEGER NOT NULL,
  stars_count INTEGER,
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL,
  UNIQUE KEY (project_name)
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS preference (
  name VARCHAR(32) NOT NULL,
  value VARCHAR(128) NOT NULL DEFAULT '',
  PRIMARY KEY (name)
);
