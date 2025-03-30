import { v4 as uuid } from "uuid";

export default class Product {
  constructor(imgSrc, alt, title, type) {
    this.id = uuid();
    this.imgSrc = imgSrc;
    this.alt = alt;
    this.title = title;
    this.type = type;
  }
}
