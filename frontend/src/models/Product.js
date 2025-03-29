import { v4 as uuid } from "uuid";

export default class Product {
  constructor(imgSrc, alt) {
    this.id = uuid();
    this.imgSrc = imgSrc;
    this.alt = alt;
  }
}
