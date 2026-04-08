import type { Principal } from "@icp-sdk/core/principal";
export interface Some<T> {
    __kind__: "Some";
    value: T;
}
export interface None {
    __kind__: "None";
}
export type Option<T> = Some<T> | None;
export interface PropertySubmission {
    locationAdvantages: string;
    propertyCategory: PropertyCategory;
    projectName: string;
    clientName: string;
    askingPrice: string;
    area: string;
    city: string;
    targetBuyer: TargetBuyer;
    floorDetails: string;
    connectivity: string;
    submittedAt: bigint;
    authorized: boolean;
    keyHighlights: string;
    email: string;
    propertyAge: string;
    contactNumber: string;
    configuration: string;
    fullAddress: string;
    priceJustification: string;
    uniqueFeatures: string;
    furnishing: Furnishing;
    location: string;
    representation: Representation;
    negotiable: boolean;
}
export enum Furnishing {
    Furnished = "Furnished",
    SemiFurnished = "SemiFurnished",
    Unfurnished = "Unfurnished"
}
export enum PropertyCategory {
    Commercial = "Commercial",
    Plot = "Plot",
    BuilderFloor = "BuilderFloor",
    Villa = "Villa",
    Apartment = "Apartment"
}
export enum Representation {
    ChannelPartner = "ChannelPartner",
    Broker = "Broker",
    Owner = "Owner"
}
export enum TargetBuyer {
    Investor = "Investor",
    PremiumBuyer = "PremiumBuyer",
    EndUser = "EndUser"
}
export interface backendInterface {
    getSubmissions(): Promise<Array<PropertySubmission>>;
    submitProperty(submission: PropertySubmission): Promise<string>;
}
