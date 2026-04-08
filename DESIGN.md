# Design Brief — Club Invest Gurgaon

## Aesthetic
Premium luxury real estate broker interface — refined, exclusive, dark-themed with warm gold accents. Sophisticated, editorial, high-end market positioning for ₹2 Cr+ properties.

## Theme
Dark mode (default). Deep charcoal backgrounds (#1f1f1f, L: 0.12) with white/cream text (L: 0.93). Gold/amber accents (H: 55°, C: 0.25) for CTAs, highlights, and interactive states. Minimal borders with generous spacing.

## Typography
| Layer | Font | Usage |
|-------|------|-------|
| Display | Instrument Serif (italic) | Page titles, section headers — elegant, timeless |
| Body | Plus Jakarta Sans | Form fields, content, descriptions — clean, readable |
| Mono | JetBrains Mono | Property details, pricing, reference numbers |

## Color Palette
| Token | Light (OKLCH) | Dark (OKLCH) | Usage |
|-------|---|---|---|
| Primary | 0.52 0.18 45 | 0.68 0.25 55 | Gold/amber: buttons, highlights, active states |
| Secondary | 0.88 0.04 45 | 0.20 0 0 | Warm grey: secondary accents, disabled states |
| Accent | 0.62 0.22 48 | 0.70 0.28 53 | Bright gold: emphasis, hover states |
| Background | 0.98 0 0 | 0.12 0 0 | Off-white (light) / deep charcoal (dark) |
| Foreground | 0.25 0 0 | 0.93 0 0 | Dark text (light) / light text (dark) |
| Card | 0.99 0 0 | 0.16 0 0 | Form sections, elevated surfaces |
| Border | 0.88 0 0 | 0.25 0 0 | Subtle dividers and outlines |
| Muted | 0.92 0.01 0 | 0.20 0 0 | Secondary text, disabled elements |

## Structural Zones
| Zone | Treatment | Purpose |
|------|-----------|---------|
| Header | Dark background (L: 0.12), firm branding, page title | Anchor and identify exclusive listing portal |
| Form Sections | Card-elevated (bg-card, shadow-elevated) with section dividers | Clear visual separation of 7-section form |
| Form Fields | input-premium: input background (L: 0.22), light border, gold focus ring | Premium input experience with accent highlights |
| CTA Buttons | btn-primary: gold background, white text, elevated shadow | Prominent submission action |
| Footer | Muted background (L: 0.20), border-top, small text | Minimum property value disclaimer |

## Spacing & Rhythm
Generous whitespace (16px, 24px, 32px gaps). Card-based sections with 24px vertical dividers. Form labels use 8px bottom margin. Dense typography hierarchy: H1 (display font, 3rem), H2 (display font, 1.875rem), body (1rem), label (0.875rem, uppercase, tracking-wide).

## Component Patterns
- **Elevated Cards**: bg-card, shadow-elevated, border-border, rounded-lg — all form sections
- **Premium Inputs**: input-premium class — 4px padding, border focus ring with accent color, placeholder in muted foreground
- **Section Headers**: Display font (italic serif), gold accent color, underline with accent border-b
- **Dividers**: section-divider — border-top border-border with vertical margin 32px
- **Checkboxes**: Accent color on checked state, rounded appearance

## Motion
Smooth transitions (all 0.3s cubic-bezier) on interactive elements. Hover opacity changes on buttons (90%). Focus rings (2px accent color) on inputs. No complex animations — restraint befits luxury positioning.

## Constraints
- No gradients or glow effects — solid colors maintain premium feel
- Minimal rounded corners (8px default)
- No bounce or playful animations
- High contrast text (AA+) on all backgrounds
- Icons use gold accent for primary actions
- Responsive: mobile-first, scaling to larger breakpoints

## Signature Detail
Gold accent color (OKLCH 0.68 0.25 55 in dark mode) applied consistently to CTAs, focus rings, and section highlights. Warm gold tone creates exclusive, luxury real estate aesthetic while maintaining readability. Property listing form becomes a "premium submission portal" rather than generic form — every detail reinforces ₹2 Cr+ market positioning.

