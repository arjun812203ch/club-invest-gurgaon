import { Toaster } from "sonner";
import SubmissionForm from "./pages/SubmissionForm";

export default function App() {
  return (
    <div
      className="min-h-screen flex flex-col bg-background text-foreground"
      style={{ fontFamily: "'Inter', sans-serif" }}
    >
      <Toaster
        position="top-right"
        theme="light"
        toastOptions={{
          style: {
            background: "#ffffff",
            border: "1px solid oklch(0.74 0.13 75 / 0.50)",
            color: "oklch(0.16 0.02 305)",
            fontFamily: "'Inter', sans-serif",
            boxShadow: "0 4px 20px oklch(0.74 0.13 75 / 0.15)",
          },
        }}
      />

      {/* Header */}
      <header
        className="sticky top-0 z-50 bg-white"
        style={{ borderBottom: "1.5px solid oklch(0.74 0.13 75 / 0.65)" }}
        data-ocid="header-nav"
      >
        {/* Logo row */}
        <div className="max-w-5xl mx-auto px-4 sm:px-6 py-3 flex items-center justify-between">
          {/* Logo Image */}
          <div className="flex items-center">
            <img
              src="/assets/cig-logo.jpeg"
              alt="Club Invest Gurgaon"
              className="object-contain"
              style={{
                maxHeight: "88px",
                maxWidth: "220px",
                width: "auto",
              }}
            />
          </div>

          {/* Tagline */}
          <div className="hidden sm:block text-right">
            <p
              style={{
                fontFamily: "'Inter', sans-serif",
                fontSize: "10px",
                letterSpacing: "0.18em",
                textTransform: "uppercase",
                fontWeight: 600,
                color: "oklch(0.60 0.15 75)",
              }}
            >
              Private Real Estate Advisor
            </p>
            <p
              style={{
                fontFamily: "'Inter', sans-serif",
                fontSize: "11px",
                letterSpacing: "0.04em",
                color: "oklch(0.35 0.30 305)",
                marginTop: "2px",
              }}
            >
              Arjun Choudhary · +91 9268215082
            </p>
          </div>
        </div>

        {/* Gurgaon Skyline panorama strip */}
        <div
          style={{
            paddingTop: "8px",
            paddingBottom: "8px",
            borderTop: "1px solid oklch(0.74 0.13 75 / 0.18)",
          }}
          aria-hidden="true"
        >
          <img
            src="/assets/gurgaon-skyline.svg"
            alt="Gurgaon skyline"
            className="w-full block"
            style={{
              maxHeight: "90px",
              objectFit: "contain",
              objectPosition: "center bottom",
            }}
          />
        </div>
      </header>

      {/* Hero Strip */}
      <div
        className="py-16 sm:py-20 px-4"
        style={{
          background:
            "linear-gradient(160deg, #ffffff 0%, oklch(0.97 0.025 305) 50%, #ffffff 100%)",
          borderBottom: "1px solid oklch(0.74 0.13 75 / 0.35)",
        }}
      >
        <div className="max-w-3xl mx-auto text-center space-y-6">
          {/* Gold ornamental line */}
          <div className="flex items-center justify-center gap-4">
            <div
              className="h-px flex-1 max-w-[100px]"
              style={{
                background:
                  "linear-gradient(to right, transparent, oklch(0.74 0.13 75))",
              }}
            />
            <p
              style={{
                fontFamily: "'Inter', sans-serif",
                fontSize: "9px",
                letterSpacing: "0.38em",
                textTransform: "uppercase",
                fontWeight: 600,
                color: "oklch(0.60 0.15 75)",
              }}
            >
              Exclusive · Curated · Confidential
            </p>
            <div
              className="h-px flex-1 max-w-[100px]"
              style={{
                background:
                  "linear-gradient(to left, transparent, oklch(0.74 0.13 75))",
              }}
            />
          </div>

          <h1
            style={{
              fontFamily: "'Inter', sans-serif",
              fontWeight: 900,
              fontSize: "clamp(2.4rem, 6vw, 4.5rem)",
              lineHeight: 1.08,
              letterSpacing: "-0.01em",
              color: "oklch(0.35 0.30 305)",
            }}
          >
            Elite Real Estate Listing
            <span
              className="block mt-2"
              style={{
                fontStyle: "italic",
                fontWeight: 700,
                fontSize: "clamp(1.8rem, 4.5vw, 3.2rem)",
                color: "oklch(0.60 0.15 75)",
                letterSpacing: "0.01em",
              }}
            >
              — Private Submission
            </span>
          </h1>

          {/* Gold divider */}
          <div className="gold-divider max-w-xs mx-auto" />

          <p
            style={{
              fontFamily: "'Inter', sans-serif",
              fontWeight: 400,
              fontSize: "clamp(0.9rem, 2vw, 1.05rem)",
              lineHeight: 1.7,
              letterSpacing: "0.03em",
              color: "oklch(0.50 0.05 305)",
              maxWidth: "520px",
              margin: "0 auto",
            }}
          >
            Exclusive placement for properties valued at{" "}
            <span style={{ fontWeight: 700, color: "oklch(0.55 0.15 70)" }}>
              ₹2 Cr+
            </span>{" "}
            in Gurgaon's most prestigious developments. Standard brokerage:{" "}
            <span style={{ fontWeight: 700, color: "oklch(0.55 0.15 70)" }}>
              2%
            </span>
            .
          </p>
        </div>
      </div>

      {/* Main Content */}
      <main className="flex-1 bg-background px-4 py-12">
        <div className="max-w-3xl mx-auto">
          <SubmissionForm />
        </div>
      </main>

      {/* Footer */}
      <footer
        className="py-12 px-4"
        style={{
          background: "oklch(0.975 0.004 90)",
          borderTop: "1.5px solid oklch(0.74 0.13 75 / 0.50)",
        }}
        data-ocid="footer"
      >
        <div className="max-w-5xl mx-auto space-y-6">
          {/* Gold ornament */}
          <div className="gold-divider max-w-sm mx-auto" />

          {/* Disclaimer */}
          <div className="text-center">
            <p
              style={{
                fontFamily: "'Inter', sans-serif",
                fontSize: "12px",
                fontStyle: "italic",
                letterSpacing: "0.05em",
                color: "oklch(0.60 0.15 75)",
              }}
            >
              ⚠ Only listings meeting the above criteria of ₹2 Cr+ value will be
              considered.
            </p>
          </div>

          {/* Contact Info */}
          <div
            className="flex flex-col sm:flex-row items-center justify-center gap-3"
            style={{
              fontFamily: "'Inter', sans-serif",
              fontSize: "13px",
              letterSpacing: "0.04em",
              color: "oklch(0.50 0.05 305)",
            }}
          >
            <span
              style={{
                fontWeight: 700,
                color: "oklch(0.16 0.02 305)",
                letterSpacing: "0.08em",
              }}
            >
              Arjun Choudhary
            </span>
            <span
              className="hidden sm:inline"
              style={{ color: "oklch(0.74 0.13 75 / 0.70)" }}
            >
              |
            </span>
            <span>569 Sector-42, Gurgaon – 122002</span>
            <span
              className="hidden sm:inline"
              style={{ color: "oklch(0.74 0.13 75 / 0.70)" }}
            >
              |
            </span>
            <a
              href="tel:+919268215082"
              className="transition-smooth hover:opacity-70"
              style={{ color: "oklch(0.35 0.30 305)", fontStyle: "italic" }}
            >
              +91 9268215082
            </a>
            <span
              className="hidden sm:inline"
              style={{ color: "oklch(0.74 0.13 75 / 0.70)" }}
            >
              |
            </span>
            <a
              href="mailto:Arjun812203ch@gmail.com"
              className="transition-smooth hover:opacity-70"
              style={{ color: "oklch(0.35 0.30 305)", fontStyle: "italic" }}
            >
              Arjun812203ch@gmail.com
            </a>
          </div>

          {/* Branding */}
          <div
            className="text-center pt-5"
            style={{ borderTop: "1px solid oklch(0.74 0.13 75 / 0.28)" }}
          >
            <p
              style={{
                fontFamily: "'Inter', sans-serif",
                fontSize: "11px",
                letterSpacing: "0.04em",
                color: "oklch(0.50 0.05 305)",
              }}
            >
              © {new Date().getFullYear()} Club Invest Gurgaon. Built with love
              using{" "}
              <a
                href={`https://caffeine.ai?utm_source=caffeine-footer&utm_medium=referral&utm_content=${encodeURIComponent(typeof window !== "undefined" ? window.location.hostname : "")}`}
                target="_blank"
                rel="noopener noreferrer"
                className="transition-smooth hover:opacity-70"
                style={{ color: "oklch(0.35 0.30 305)", fontStyle: "italic" }}
              >
                caffeine.ai
              </a>
            </p>
          </div>
        </div>
      </footer>
    </div>
  );
}
