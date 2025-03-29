const express = require('express');
const mongoose = require('mongoose');
require('dotenv').config();

// Initialize the app
const app = express();

// Middleware to parse incoming JSON data
app.use(express.json());

// MongoDB connection

// Basic route
app.get('/', (req, res) => {
  res.send('Hello, World!');
});

// Server listening
const PORT = process.env.PORT || 4000;
app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
});
