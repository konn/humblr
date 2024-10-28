INSERT INTO articles (body, createdAt, lastUpdate, slug) 
  VALUES ("This is my **first** blog post"
    , "2024-07-27T07:00:00Z", "2024-07-27T07:00:00Z", "2024-07-27-1605-my-first-blog");

INSERT INTO tags (name) VALUES ("アナウンス");
INSERT INTO tags (name) VALUES ("タグ2");

INSERT INTO articleTags (article, tag) VALUES (1, 1);
