const sharp = require('sharp');
const axios = require('axios');
const fs = require('fs');

const fetchImageFromFacePlusPlus = async (imageId) => {
    try {
      const response = await axios({
        method: 'get',
        url: facePlusPlusAPI,
        params: {
          api_key: apiKey,
          api_secret: apiSecret,
          image_id: imageId,
        },
        responseType: 'arraybuffer', // Receive image as binary data
      });
  
      return response.data; // Return the image binary data
    } catch (error) {
      console.error('Error fetching image from Face++:', error);
      throw error;
    }
  };