const express = require('express');
const path = require('path'); 
const FormData = require('form-data');
const cors = require('cors'); 
const axios = require('axios');
const fs = require('fs');
const crypto = require('crypto');
const sharp = require('sharp');
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
app.use('/img', express.static(path.join(__dirname, 'img')));

// Basic route
app.get('/', (req, res) => {
  res.send('Hello, World!');
});

/* Will take a JSON containing a faceDetectionResponse (ie. output of detect-face) as well as a given 
foundationColor and opacity, and overlay a foundation rectangle of that color and transparency on top 
of the face provided in the image using the applyFoundation API. */
app.post('/apply-foundation', async (req, res) => {
    const { faceDetectionResponse, foundationColor, opacity } = req.body;
  
    if (!faceDetectionResponse || !faceDetectionResponse.faces || faceDetectionResponse.faces.length === 0) {
      return res.status(400).json({ error: 'No faces detected' });
    }
  
    const face = faceDetectionResponse.faces[0];  // assuming we only have one face detected
    const { face_token, face_rectangle } = face;
  
    try {
      // fetch the image using the imageId (face_token)
      const imageId = faceDetectionResponse.image_id;
      const imageBuffer = await fetchImageFromFacePlusPlus(imageId);
  
      // apply foundation to the image based on the face's coordinates
      const processedImage = await applyFoundation(imageBuffer, face_rectangle, foundationColor, opacity);
    
      console.log("successfully proccessed the image", processedImage)
      // send the processed image back to the client
      res.type('png').send(processedImage);
    } catch (error) {
      console.error('Error processing the image:', error);
      res.status(500).json({ error: 'Error applying foundation' });
    }
});

// Load image from local storage
const fetchImageFromFacePlusPlus = async (imageId) => {
    const imagePath = path.join(__dirname, 'img', `${imageId}.jpg`);

    console.log(`Looking for image at: ${imagePath}`); // Debugging
  
    try {
        // Check if the file exists
        if (!fs.existsSync(imagePath)) {
            console.error(`Image not found at path: ${imagePath}`);
            throw new Error('Image file not found');
        }
        
        const imageBuffer = fs.readFileSync(imagePath);
        console.log("Found image buffer", imageBuffer);
        return Buffer.from(imageBuffer);
    } catch (error) {
        throw new Error('Error loading image from local storage');
    }
};

// Route to save the image locally
app.post('/save-image', async (req, res) => {
    const { imageData } = req.body;
  
    if (!imageData) {
      return res.status(400).json({ error: 'No image data provided' });
    }
  
    // Generate a unique image ID
    const imageId = crypto.randomBytes(16).toString('hex');
    const base64Data = imageData.replace(/^data:image\/jpeg;base64,/, '');
    const imagePath = path.join(__dirname, 'img', `${imageId}.jpg`);
  
    // Save the image locally
    fs.writeFile(imagePath, base64Data, 'base64', (err) => {
      if (err) {
        console.error('Error saving image:', err);
        return res.status(500).json({ error: 'Error saving image' });
      }
      res.json({ imageId });
    });
});

// Apply foundation to the detected face area
const applyFoundation = async (imageBuffer, faceRectangle, foundationColor, opacity) => {
    try {
      console.log('✅ Face rectangle received:', faceRectangle);
      console.log('✅ Foundation color:', foundationColor);
      console.log('✅ Opacity:', opacity);
  
      // Check if faceRectangle dimensions are valid
      if (!faceRectangle || !faceRectangle.width || !faceRectangle.height) {
        console.error('❌ Invalid faceRectangle:', faceRectangle);
        throw new Error('Invalid faceRectangle dimensions');
      }
  
      // Create the overlay with Sharp
      const overlay = sharp({
        create: {
          width: faceRectangle.width,
          height: faceRectangle.height,
          channels: 4, // RGBA
          background: {
            r: foundationColor.r,
            g: foundationColor.g,
            b: foundationColor.b,
            alpha: opacity,
          },
        },
      }).png();
  
      console.log('✅ Created the overlay');
  
      // Composite the overlay onto the image using the detected face coordinates
      const processedImage = await sharp(imageBuffer)
        .composite([
          {
            input: await overlay.toBuffer(), // Convert the overlay to a buffer
            left: faceRectangle.left,
            top: faceRectangle.top,
          },
        ])
        .toBuffer();
  
      console.log('✅ Foundation applied successfully');
      return processedImage; // Return the processed image
    } catch (error) {
      console.error('❌ Error applying foundation:', error);
      throw new Error('Error applying foundation to image');
    }
  };
  

// Route for face detection
app.post('/detect-face', async (req, res) => {
    const { imageUrl, imageId } = req.body; // Add imageId here
  
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
  
      // Include imageId in the response for applyFoundation
      res.json({
        ...response.data,
        image_id: imageId,
      });
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