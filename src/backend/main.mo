import Types "types/property-listings";
import PropertyListingsMixin "mixins/property-listings-api";
import List "mo:core/List";

actor {
  let submissions = List.empty<Types.PropertySubmission>();
  let submissionCounter = { var value : Nat = 0 };

  include PropertyListingsMixin(submissions, submissionCounter);
};
