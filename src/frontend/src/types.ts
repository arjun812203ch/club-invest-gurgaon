export interface PropertyFormData {
  // Section 1: Client Profile
  clientName: string;
  contactNumber: string;
  email: string;
  representation: "self" | "agent" | "poa";

  // Section 2: Property Details
  propertyCategory:
    | "residential"
    | "commercial"
    | "villa"
    | "penthouse"
    | "plot";
  projectName: string;
  city: string;
  location: string;
  fullAddress: string;

  // Section 3: Specifications
  area: string;
  configuration: string;
  floorDetails: string;
  furnishing: "unfurnished" | "semi-furnished" | "fully-furnished";
  propertyAge: string;

  // Section 4: Pricing
  askingPrice: number;
  priceJustification: string;
  negotiable: boolean;

  // Section 5: Positioning
  targetBuyer: string;
  locationAdvantages: string;
  connectivity: string;

  // Section 6: Unique Selling Points
  keyHighlights: string;
  uniqueFeatures: string;

  // Section 7: Authorization
  authorized: boolean;
}
