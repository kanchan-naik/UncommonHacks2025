import { v4 as uuid } from "uuid";

export default class Product {
  constructor(imgSrc, alt, title) {
    this.id = uuid();
    this.imgSrc = imgSrc;
    this.alt = alt;
    this.title = title;
  }
}
