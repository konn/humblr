CREATE TABLE IF NOT EXISTS articles (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  body TEXT,
  createdAt TEXT NOT NULL,
  lastUpdate TEXT NOT NULL,
  slug TEXT NOT NULL UNIQUE
);
CREATE TABLE IF NOT EXISTS tags (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE
);
CREATE TABLE IF NOT EXISTS articleTags (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  article INTEGER NOT NULL,
  tag INTEGER NOT NULL,
  FOREIGN KEY (article) REFERENCES articles(id),
  FOREIGN KEY (tag) REFERENCES tags(id)
);
CREATE TABLE IF NOT EXISTS articleImages (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  article INTEGER NOT NULL,
  image_ INTEGER NOT NULL,
  offset INTEGER NOT NULL,
  FOREIGN KEY (article) REFERENCES articles(id),
  FOREIGN KEY (image_) REFERENCES images(id),
  CONSTRAINT unique_image_offset UNIQUE (article, image_, offset)
);
CREATE TABLE IF NOT EXISTS images (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  link TEXT NOT NULL,
  ctype TEXT NOT NULL,
  CONSTRAINT unique_image UNIQUE (link)
);
