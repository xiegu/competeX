// initialize an introjs instance          
var intro = introJs();

// handler 1
Shiny.addCustomMessageHandler("setHelpContent1",
                              
                              // callback function. 
                              // note: message is passed by shiny and contains the tour data
                              function(message){
                                
                                // load data 
                                intro.setOptions({steps: message.steps_1 });
                                
                              }
                              
);

Shiny.addCustomMessageHandler("setHelpContent2",
                              
                              // callback function. 
                              // note: message is passed by shiny and contains the tour data
                              function(message){
                                
                                // load data 
                                intro.setOptions({steps: message.steps_2 });
                                
                              }
                              
);

Shiny.addCustomMessageHandler("setHelpContent3",
                              
                              // callback function. 
                              // note: message is passed by shiny and contains the tour data
                              function(message){
                                
                                // load data 
                                intro.setOptions({steps: message.steps_3 });
                                
                              }
                              
);

Shiny.addCustomMessageHandler("setHelpContent4",
                              
                              // callback function. 
                              // note: message is passed by shiny and contains the tour data
                              function(message){
                                
                                // load data 
                                intro.setOptions({steps: message.steps_4 });
                                
                              }
                              
);

Shiny.addCustomMessageHandler("setHelpContent5",
                              
                              // callback function. 
                              // note: message is passed by shiny and contains the tour data
                              function(message){
                                
                                // load data 
                                intro.setOptions({steps: message.steps_5 });
                                
                              }
                              
);
// handler 2
Shiny.addCustomMessageHandler("startHelp1",
                              
                              // callback function
                              function(message) {
                                
                                // start intro.js
                                // note: we don't need information from shiny, just start introJS
    intro.start();
  }
  
);

Shiny.addCustomMessageHandler("startHelp2",
                              
                              // callback function
                              function(message) {
                                
                                // start intro.js
                                // note: we don't need information from shiny, just start introJS
    intro.start();
  }
  
);

Shiny.addCustomMessageHandler("startHelp3",
                              
                              // callback function
                              function(message) {
                                
                                // start intro.js
                                // note: we don't need information from shiny, just start introJS
    intro.start();
  }
  
);

Shiny.addCustomMessageHandler("startHelp4",
                              
                              // callback function
                              function(message) {
                                
                                // start intro.js
                                // note: we don't need information from shiny, just start introJS
    intro.start();
  }
  
);

Shiny.addCustomMessageHandler("startHelp5",
                              
                              // callback function
                              function(message) {
                                
                                // start intro.js
                                // note: we don't need information from shiny, just start introJS
    intro.start();
  }
  
);