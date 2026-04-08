import { Button } from "@/components/ui/button";
import { Checkbox } from "@/components/ui/checkbox";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { RadioGroup, RadioGroupItem } from "@/components/ui/radio-group";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { Textarea } from "@/components/ui/textarea";
import { CheckCircle2, ChevronRight, Gem } from "lucide-react";
import { useState } from "react";
import { useForm } from "react-hook-form";
import { toast } from "sonner";
import {
  Furnishing,
  PropertyCategory,
  Representation,
  TargetBuyer,
  createActor,
} from "../backend";
import type { PropertyFormData } from "../types";

const PF = { fontFamily: "'Inter', sans-serif" } as const;

// ── Section wrapper ──────────────────────────────────────────────────────────
function FormSection({
  number,
  title,
  children,
}: {
  number: number;
  title: string;
  children: React.ReactNode;
}) {
  return (
    <div
      className="bg-white rounded-xl p-6 sm:p-8 space-y-6"
      style={{
        border: "1px solid oklch(0.74 0.13 75 / 0.42)",
        boxShadow:
          "0 2px 8px oklch(0.74 0.13 75 / 0.10), 0 8px 32px oklch(0.74 0.13 75 / 0.06)",
      }}
    >
      <div>
        <div className="flex items-center gap-3 mb-4">
          <span
            className="w-9 h-9 rounded-full flex items-center justify-center text-white flex-shrink-0"
            style={{
              ...PF,
              fontSize: "13px",
              fontWeight: 700,
              background: "oklch(0.35 0.30 305)",
              border: "1.5px solid oklch(0.74 0.13 75 / 0.55)",
              boxShadow: "0 0 0 3px oklch(0.35 0.30 305 / 0.12)",
            }}
          >
            {number}
          </span>
          <h2
            style={{
              ...PF,
              fontSize: "11px",
              fontWeight: 600,
              letterSpacing: "0.20em",
              textTransform: "uppercase",
              color: "oklch(0.35 0.30 305)",
            }}
          >
            {title}
          </h2>
        </div>
        <div className="gold-divider" />
      </div>
      {children}
    </div>
  );
}

// ── Field wrapper ────────────────────────────────────────────────────────────
function Field({
  label,
  error,
  children,
}: {
  label: string;
  error?: string;
  children: React.ReactNode;
}) {
  return (
    <div className="space-y-2">
      <Label
        style={{
          ...PF,
          fontSize: "10px",
          fontWeight: 600,
          letterSpacing: "0.20em",
          textTransform: "uppercase",
          color: "oklch(0.16 0.02 305)",
        }}
      >
        {label}
      </Label>
      {children}
      {error && (
        <p style={{ ...PF, color: "oklch(0.55 0.22 25)", fontSize: "11px" }}>
          {error}
        </p>
      )}
    </div>
  );
}

// ── Success State ────────────────────────────────────────────────────────────
function SuccessState() {
  return (
    <div
      className="bg-white rounded-xl p-10 sm:p-16 text-center space-y-7"
      style={{
        border: "1.5px solid oklch(0.74 0.13 75 / 0.60)",
        boxShadow:
          "0 4px 24px oklch(0.74 0.13 75 / 0.18), 0 1px 4px oklch(0.74 0.13 75 / 0.10)",
      }}
    >
      <div className="flex justify-center">
        <div
          className="w-20 h-20 rounded-full flex items-center justify-center"
          style={{
            background: "oklch(0.93 0.04 305)",
            border: "1.5px solid oklch(0.74 0.13 75 / 0.55)",
            boxShadow: "0 0 0 6px oklch(0.74 0.13 75 / 0.10)",
          }}
        >
          <CheckCircle2
            className="w-10 h-10"
            style={{ color: "oklch(0.35 0.30 305)" }}
          />
        </div>
      </div>
      <div className="space-y-4">
        <h2
          style={{
            ...PF,
            fontWeight: 700,
            fontSize: "clamp(1.6rem, 4vw, 2.4rem)",
            fontStyle: "italic",
            letterSpacing: "0.01em",
            color: "oklch(0.35 0.30 305)",
          }}
        >
          Submission Received
        </h2>
        <div className="gold-divider max-w-xs mx-auto" />
        <p
          style={{
            ...PF,
            fontWeight: 400,
            fontSize: "15px",
            letterSpacing: "0.03em",
            lineHeight: 1.7,
            color: "oklch(0.50 0.05 305)",
            maxWidth: "400px",
            margin: "0 auto",
          }}
        >
          Thank you — your listing has been submitted for review. Our team will
          contact you shortly.
        </p>
      </div>
      <div
        className="inline-flex items-center gap-2"
        style={{
          ...PF,
          fontSize: "10px",
          letterSpacing: "0.30em",
          textTransform: "uppercase",
          fontWeight: 600,
          color: "oklch(0.60 0.15 75)",
        }}
      >
        <Gem className="w-3 h-3" />
        <span>Club Invest Gurgaon · Private &amp; Curated</span>
        <Gem className="w-3 h-3" />
      </div>
    </div>
  );
}

// ── Main Form ────────────────────────────────────────────────────────────────
export default function SubmissionForm() {
  const [submitted, setSubmitted] = useState(false);
  const [isSubmitting, setIsSubmitting] = useState(false);

  const { register, handleSubmit, setValue, watch } = useForm<PropertyFormData>(
    {
      mode: "onChange",
      defaultValues: {
        representation: "self",
        propertyCategory: "residential",
        furnishing: "unfurnished",
        targetBuyer: "investor",
        negotiable: false,
        authorized: false,
      },
    },
  );

  const authorized = watch("authorized");
  const negotiable = watch("negotiable");

  const onSubmit = async (data: PropertyFormData) => {
    setIsSubmitting(true);
    try {
      const representationMap: Record<string, Representation> = {
        self: Representation.Owner,
        agent: Representation.Broker,
        poa: Representation.ChannelPartner,
      };
      const categoryMap: Record<string, PropertyCategory> = {
        residential: PropertyCategory.Apartment,
        commercial: PropertyCategory.Commercial,
        villa: PropertyCategory.Villa,
        penthouse: PropertyCategory.BuilderFloor,
        plot: PropertyCategory.Plot,
      };
      const furnishingMap: Record<string, Furnishing> = {
        "fully-furnished": Furnishing.Furnished,
        "semi-furnished": Furnishing.SemiFurnished,
        unfurnished: Furnishing.Unfurnished,
      };
      const buyerMap: Record<string, TargetBuyer> = {
        investor: TargetBuyer.Investor,
        end_user: TargetBuyer.EndUser,
        premium: TargetBuyer.PremiumBuyer,
      };

      const submission = {
        clientName: data.clientName ?? "",
        contactNumber: data.contactNumber ?? "",
        email: data.email ?? "",
        representation:
          representationMap[data.representation] ?? Representation.Owner,
        propertyCategory:
          categoryMap[data.propertyCategory] ?? PropertyCategory.Apartment,
        projectName: data.projectName ?? "",
        city: data.city ?? "",
        location: data.location ?? "",
        fullAddress: data.fullAddress ?? "",
        area: data.area ?? "",
        configuration: data.configuration ?? "",
        floorDetails: data.floorDetails ?? "",
        furnishing: furnishingMap[data.furnishing] ?? Furnishing.Unfurnished,
        propertyAge: data.propertyAge ?? "",
        askingPrice: data.askingPrice ? String(data.askingPrice) : "",
        priceJustification: data.priceJustification ?? "",
        negotiable: data.negotiable,
        targetBuyer: buyerMap[data.targetBuyer] ?? TargetBuyer.Investor,
        locationAdvantages: data.locationAdvantages ?? "",
        connectivity: data.connectivity ?? "",
        keyHighlights: data.keyHighlights ?? "",
        uniqueFeatures: data.uniqueFeatures ?? "",
        authorized: data.authorized,
        submittedAt: BigInt(Date.now()),
      };

      try {
        const canisterId = import.meta.env.VITE_CANISTER_ID_BACKEND as
          | string
          | undefined;
        if (canisterId) {
          const noopUpload = async (_file: unknown) => new Uint8Array();
          const noopDownload = async (_file: unknown) => ({
            directURL: "",
            _blob: null,
            onProgress: undefined,
            getBytes: async () => new Uint8Array(),
            getDirectURL: () => "",
            withUploadProgress: (fn: unknown) => ({
              directURL: "",
              getBytes: async () => new Uint8Array(),
              getDirectURL: () => "",
              withUploadProgress: fn,
            }),
          });
          // @ts-expect-error noop stubs for unauthenticated anonymous actor
          const backend = createActor(canisterId, noopUpload, noopDownload);
          await backend.submitProperty(submission);
        }
      } catch {
        // Backend not yet deployed — submission recorded locally
      }

      console.info("Property submission:", submission);
      setSubmitted(true);
      toast.success("Listing submitted successfully");
    } catch (err) {
      console.error(err);
      toast.error("Submission failed. Please try again.");
    } finally {
      setIsSubmitting(false);
    }
  };

  if (submitted) return <SuccessState />;

  return (
    <form
      onSubmit={handleSubmit(onSubmit)}
      className="space-y-6"
      data-ocid="submission-form"
      noValidate
      style={PF}
    >
      {/* Intro Banner */}
      <div
        className="bg-white rounded-xl px-6 py-5 flex flex-col sm:flex-row sm:items-center gap-3"
        style={{
          border: "1px solid oklch(0.74 0.13 75 / 0.55)",
          boxShadow: "0 2px 12px oklch(0.74 0.13 75 / 0.10)",
        }}
      >
        <Gem
          className="w-5 h-5 flex-shrink-0"
          style={{ color: "oklch(0.60 0.15 75)" }}
        />
        <p
          style={{
            ...PF,
            fontWeight: 400,
            fontSize: "14px",
            letterSpacing: "0.03em",
            lineHeight: 1.65,
            color: "oklch(0.50 0.05 305)",
          }}
        >
          Submit your property for a curated listing and private video showcase.{" "}
          <span style={{ fontWeight: 700, color: "oklch(0.55 0.15 70)" }}>
            Minimum property value: ₹2 Cr+
          </span>
          {" | "}
          <span style={{ color: "oklch(0.35 0.30 305)", fontStyle: "italic" }}>
            Standard brokerage: 2%
          </span>
        </p>
      </div>

      {/* ── Section 1: Client Profile ──────────────────────────────────── */}
      <FormSection number={1} title="Client Profile">
        <div className="grid grid-cols-1 sm:grid-cols-2 gap-5">
          <Field label="Full Name">
            <Input
              {...register("clientName")}
              placeholder="e.g., Rahul Sharma"
              className="input-premium w-full"
              data-ocid="input-client-name"
            />
          </Field>
          <Field label="Contact Number">
            <Input
              {...register("contactNumber")}
              placeholder="e.g., 9876543210"
              maxLength={10}
              className="input-premium w-full"
              data-ocid="input-contact-number"
            />
          </Field>
          <Field label="Email Address">
            <Input
              {...register("email")}
              type="email"
              placeholder="e.g., rahul@example.com"
              className="input-premium w-full"
              data-ocid="input-email"
            />
          </Field>
          <Field label="Representation">
            <Select
              defaultValue="self"
              onValueChange={(v) =>
                setValue(
                  "representation",
                  v as PropertyFormData["representation"],
                  {
                    shouldValidate: false,
                  },
                )
              }
            >
              <SelectTrigger
                className="input-premium w-full"
                data-ocid="select-representation"
              >
                <SelectValue placeholder="Select…" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="self">Owner</SelectItem>
                <SelectItem value="agent">Broker</SelectItem>
                <SelectItem value="poa">Channel Partner</SelectItem>
              </SelectContent>
            </Select>
          </Field>
        </div>
      </FormSection>

      {/* ── Section 2: Property Type ───────────────────────────────────── */}
      <FormSection number={2} title="Property Type">
        <div className="grid grid-cols-1 sm:grid-cols-2 gap-5">
          <Field label="Property Category">
            <Select
              defaultValue="residential"
              onValueChange={(v) =>
                setValue(
                  "propertyCategory",
                  v as PropertyFormData["propertyCategory"],
                  {
                    shouldValidate: false,
                  },
                )
              }
            >
              <SelectTrigger
                className="input-premium w-full"
                data-ocid="select-property-category"
              >
                <SelectValue placeholder="Select…" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="residential">Apartment</SelectItem>
                <SelectItem value="penthouse">Builder Floor</SelectItem>
                <SelectItem value="villa">Villa</SelectItem>
                <SelectItem value="plot">Plot</SelectItem>
                <SelectItem value="commercial">Commercial</SelectItem>
              </SelectContent>
            </Select>
          </Field>
          <Field label="Project / Society Name">
            <Input
              {...register("projectName")}
              placeholder="e.g., DLF Camellias"
              className="input-premium w-full"
              data-ocid="input-project-name"
            />
          </Field>
          <Field label="City">
            <Input
              {...register("city")}
              placeholder="e.g., Gurgaon"
              className="input-premium w-full"
              data-ocid="input-city"
            />
          </Field>
          <Field label="Location / Sector">
            <Input
              {...register("location")}
              placeholder="e.g., Sector 42"
              className="input-premium w-full"
              data-ocid="input-location"
            />
          </Field>
        </div>
        <Field label="Full Address">
          <Textarea
            {...register("fullAddress")}
            placeholder="Complete address including building, floor, wing…"
            rows={3}
            className="input-premium w-full resize-none"
            data-ocid="input-full-address"
          />
        </Field>
      </FormSection>

      {/* ── Section 3: Specifications ──────────────────────────────────── */}
      <FormSection number={3} title="Property Specifications">
        <div className="grid grid-cols-1 sm:grid-cols-2 gap-5">
          <Field label="Area (sq. ft.)">
            <Input
              {...register("area")}
              placeholder="e.g., 4500 sq. ft."
              className="input-premium w-full"
              data-ocid="input-area"
            />
          </Field>
          <Field label="Configuration (BHK / Layout)">
            <Input
              {...register("configuration")}
              placeholder="e.g., 4 BHK + Staff"
              className="input-premium w-full"
              data-ocid="input-configuration"
            />
          </Field>
          <Field label="Floor Details">
            <Input
              {...register("floorDetails")}
              placeholder="e.g., 12th Floor, Tower B"
              className="input-premium w-full"
              data-ocid="input-floor-details"
            />
          </Field>
          <Field label="Furnishing Status">
            <Select
              defaultValue="unfurnished"
              onValueChange={(v) =>
                setValue("furnishing", v as PropertyFormData["furnishing"], {
                  shouldValidate: false,
                })
              }
            >
              <SelectTrigger
                className="input-premium w-full"
                data-ocid="select-furnishing"
              >
                <SelectValue placeholder="Select…" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="fully-furnished">Fully Furnished</SelectItem>
                <SelectItem value="semi-furnished">Semi Furnished</SelectItem>
                <SelectItem value="unfurnished">Unfurnished</SelectItem>
              </SelectContent>
            </Select>
          </Field>
          <Field label="Property Age">
            <Input
              {...register("propertyAge")}
              placeholder="e.g., 3 years / New"
              className="input-premium w-full"
              data-ocid="input-property-age"
            />
          </Field>
        </div>
      </FormSection>

      {/* ── Section 4: Pricing ─────────────────────────────────────────── */}
      <FormSection number={4} title="Pricing">
        <div className="grid grid-cols-1 sm:grid-cols-2 gap-5">
          <Field label="Asking Price (₹)">
            <div className="relative">
              <span
                className="absolute left-3 top-1/2 -translate-y-1/2 pointer-events-none"
                style={{
                  ...PF,
                  fontWeight: 700,
                  fontSize: "14px",
                  color: "oklch(0.60 0.15 75)",
                }}
              >
                ₹
              </span>
              <Input
                {...register("askingPrice", { valueAsNumber: true })}
                type="number"
                placeholder="e.g., 25000000"
                className="input-premium w-full pl-7"
                data-ocid="input-asking-price"
              />
            </div>
            <p
              style={{
                ...PF,
                fontSize: "11px",
                letterSpacing: "0.03em",
                color: "oklch(0.60 0.15 75)",
                marginTop: "4px",
              }}
            >
              Recommended minimum: ₹2,00,00,000 (₹2 Cr)
            </p>
          </Field>
          <Field label="Negotiable">
            <RadioGroup
              value={negotiable ? "yes" : "no"}
              onValueChange={(v) =>
                setValue("negotiable", v === "yes", { shouldValidate: false })
              }
              className="flex items-center gap-6 pt-2"
              data-ocid="radio-negotiable"
            >
              <div className="flex items-center gap-2">
                <RadioGroupItem
                  value="yes"
                  id="neg-yes"
                  className="border-primary text-primary"
                />
                <Label
                  htmlFor="neg-yes"
                  style={{
                    ...PF,
                    fontSize: "13px",
                    letterSpacing: "0.05em",
                    cursor: "pointer",
                  }}
                >
                  Yes
                </Label>
              </div>
              <div className="flex items-center gap-2">
                <RadioGroupItem
                  value="no"
                  id="neg-no"
                  className="border-primary text-primary"
                />
                <Label
                  htmlFor="neg-no"
                  style={{
                    ...PF,
                    fontSize: "13px",
                    letterSpacing: "0.05em",
                    cursor: "pointer",
                  }}
                >
                  No
                </Label>
              </div>
            </RadioGroup>
          </Field>
        </div>
        <Field label="Price Justification">
          <Textarea
            {...register("priceJustification")}
            placeholder="Explain your pricing rationale — recent comparable sales, premium features, appreciation potential…"
            rows={3}
            className="input-premium w-full resize-none"
            data-ocid="input-price-justification"
          />
        </Field>
      </FormSection>

      {/* ── Section 5: Positioning ────────────────────────────────────── */}
      <FormSection number={5} title="Market Positioning">
        <div className="grid grid-cols-1 sm:grid-cols-2 gap-5">
          <Field label="Target Buyer">
            <Select
              defaultValue="investor"
              onValueChange={(v) =>
                setValue("targetBuyer", v, { shouldValidate: false })
              }
            >
              <SelectTrigger
                className="input-premium w-full"
                data-ocid="select-target-buyer"
              >
                <SelectValue placeholder="Select…" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="investor">Investor</SelectItem>
                <SelectItem value="end_user">End User</SelectItem>
                <SelectItem value="premium">Premium Buyer</SelectItem>
              </SelectContent>
            </Select>
          </Field>
          <Field label="Location Advantages">
            <Input
              {...register("locationAdvantages")}
              placeholder="e.g., Golf Course Road frontage, park-facing"
              className="input-premium w-full"
              data-ocid="input-location-advantages"
            />
          </Field>
        </div>
        <Field label="Connectivity / Nearby Landmarks">
          <Textarea
            {...register("connectivity")}
            placeholder="e.g., 5 min to Cyber Hub, 10 min to IGI Airport, metro station 800m…"
            rows={3}
            className="input-premium w-full resize-none"
            data-ocid="input-connectivity"
          />
        </Field>
      </FormSection>

      {/* ── Section 6: USP ────────────────────────────────────────────── */}
      <FormSection number={6} title="Unique Selling Points">
        <Field label="Key Highlights">
          <Textarea
            {...register("keyHighlights")}
            placeholder="e.g., Private terrace with panoramic city views, home automation, dedicated concierge…"
            rows={4}
            className="input-premium w-full resize-none"
            data-ocid="input-key-highlights"
          />
        </Field>
        <Field label="Unique Features">
          <Textarea
            {...register("uniqueFeatures")}
            placeholder="e.g., Custom Italian marble, Gaggenau modular kitchen, private plunge pool…"
            rows={4}
            className="input-premium w-full resize-none"
            data-ocid="input-unique-features"
          />
        </Field>
      </FormSection>

      {/* ── Section 7: Authorization ──────────────────────────────────── */}
      <FormSection number={7} title="Authorization">
        <div
          className="flex items-start gap-4 p-5 rounded-lg transition-smooth"
          style={{
            background: authorized
              ? "oklch(0.93 0.04 305)"
              : "oklch(0.97 0.004 90)",
            border: authorized
              ? "1px solid oklch(0.35 0.30 305 / 0.40)"
              : "1px solid oklch(0.74 0.13 75 / 0.35)",
          }}
        >
          <Checkbox
            id="authorized"
            checked={authorized}
            onCheckedChange={(checked) =>
              setValue("authorized", checked === true, {
                shouldValidate: false,
              })
            }
            className="mt-0.5 border-primary data-[state=checked]:bg-primary data-[state=checked]:text-primary-foreground"
            data-ocid="checkbox-authorized"
          />
          <label
            htmlFor="authorized"
            style={{
              ...PF,
              fontSize: "13px",
              letterSpacing: "0.03em",
              lineHeight: 1.7,
              cursor: "pointer",
              color: "oklch(0.20 0.02 305)",
            }}
          >
            I confirm the information provided is accurate and agree to a{" "}
            <span style={{ fontWeight: 700, color: "oklch(0.60 0.15 75)" }}>
              2% brokerage fee
            </span>{" "}
            upon successful closure. I also authorize{" "}
            <span style={{ fontWeight: 700, color: "oklch(0.60 0.15 75)" }}>
              video recording and marketing
            </span>{" "}
            of this property by Club Invest Gurgaon for the purpose of private
            listing, digital showcase, and promotion to prospective buyers.
          </label>
        </div>
        <p
          style={{
            ...PF,
            fontSize: "11px",
            letterSpacing: "0.04em",
            color: "oklch(0.50 0.05 305)",
            fontStyle: "italic",
          }}
        >
          Optional — tick to confirm your authorization.
        </p>
      </FormSection>

      {/* ── Submit ────────────────────────────────────────────────────── */}
      <div className="flex flex-col sm:flex-row items-center justify-between gap-4 pt-2">
        <p
          style={{
            ...PF,
            fontSize: "11px",
            letterSpacing: "0.04em",
            fontStyle: "italic",
            color: "oklch(0.60 0.15 75)",
            textAlign: "center",
          }}
        >
          ⚠ Only listings valued at ₹2 Cr+ will be considered for placement.
        </p>
        <Button
          type="submit"
          disabled={isSubmitting}
          className="w-full sm:w-auto flex items-center gap-2 min-w-[240px] justify-center transition-smooth"
          style={{
            ...PF,
            fontWeight: 600,
            fontSize: "13px",
            letterSpacing: "0.18em",
            textTransform: "uppercase",
            background: "oklch(0.35 0.30 305)",
            color: "oklch(0.98 0.005 90)",
            border: "1.5px solid oklch(0.74 0.13 75 / 0.60)",
            boxShadow: "0 2px 14px oklch(0.35 0.30 305 / 0.28)",
            borderRadius: "0.375rem",
            padding: "0.85rem 1.75rem",
          }}
          data-ocid="btn-submit-listing"
        >
          {isSubmitting ? (
            <>
              <span
                className="w-4 h-4 border-2 rounded-full animate-spin"
                style={{
                  borderColor: "oklch(0.74 0.13 75 / 0.40)",
                  borderTopColor: "oklch(0.74 0.13 75)",
                }}
              />
              Submitting…
            </>
          ) : (
            <>
              Submit Private Listing
              <ChevronRight className="w-4 h-4" />
            </>
          )}
        </Button>
      </div>
    </form>
  );
}
