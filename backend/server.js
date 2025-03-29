const express = require('express');
const mongoose = require('mongoose');
const path = require('path'); 
const FormData = require('form-data');
const cors = require('cors'); 
const axios = require('axios');
require('dotenv').config();

// Initialize the app
const app = express();

const API_KEY = 'sTXiB4FnvqU8Hr50A2ahVs7NqzDnB5sg'; // Face++ API Key
const API_SECRET = 'Ev5g1f_7R8G-jk--qMJj-70yNFPH9kdx'; // Face++ API Secret

// Middleware to parse incoming JSON data
app.use(cors({
    origin: 'http://localhost:3001',  // React frontend URL
    methods: ['GET', 'POST', 'OPTIONS'],  // Allow methods
    allowedHeaders: ['Content-Type'],  // Allow specific headers
  }));  
app.use(express.json());
app.use(express.static(path.join(__dirname, 'public')));

// MongoDB connection

// Basic route
app.get('/', (req, res) => {
  res.send('Hello, World!');
});

// Route for face detection
app.post('/detect-face', async (req, res) => {
    const { imageUrl } = req.body;  // Expecting base64 image URL
  
    if (!imageUrl) {
      return res.status(400).json({ error: 'No image provided' });
    }
  
    try {
      const form = new FormData();
      form.append('api_key', API_KEY);
      form.append('api_secret', API_SECRET);
      form.append('image_base64', imageUrl); // Sending base64 encoded image
  
      const response = await axios.post('https://api-us.faceplusplus.com/facepp/v3/detect', form, {
        headers: form.getHeaders(),
      });
  
      res.json(response.data);
    } catch (error) {
      console.error('Error in Face++ detection:', error);
      res.status(500).json({ error: 'Face detection failed' });
    }
  });

// Server listening
const PORT = process.env.PORT || 4000;
app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
});
