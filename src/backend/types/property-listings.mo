module {
  public type Representation = {
    #Owner;
    #Broker;
    #ChannelPartner;
  };

  public type PropertyCategory = {
    #Apartment;
    #BuilderFloor;
    #Villa;
    #Plot;
    #Commercial;
  };

  public type Furnishing = {
    #Furnished;
    #SemiFurnished;
    #Unfurnished;
  };

  public type TargetBuyer = {
    #Investor;
    #EndUser;
    #PremiumBuyer;
  };

  public type PropertySubmission = {
    // Client Profile
    clientName         : Text;
    contactNumber      : Text;
    email              : Text;
    representation     : Representation;
    // Property Details
    propertyCategory   : PropertyCategory;
    projectName        : Text;
    city               : Text;
    location           : Text;
    fullAddress        : Text;
    // Specifications
    area               : Text;
    configuration      : Text;
    floorDetails       : Text;
    furnishing         : Furnishing;
    propertyAge        : Text;
    // Pricing
    askingPrice        : Text;
    priceJustification : Text;
    negotiable         : Bool;
    // Positioning
    targetBuyer        : TargetBuyer;
    locationAdvantages : Text;
    connectivity       : Text;
    // Unique Selling Points
    keyHighlights      : Text;
    uniqueFeatures     : Text;
    // Authorization
    authorized         : Bool;
    submittedAt        : Int;
  };
};
