# -*- coding: utf-8 -*-
"""
בניית המצגת של שבוע 6 מתוך הפלטים של bpd_week6.R.

התבנית: המצגת של שבוע 5 (אותן פריסות, גופן Assistant וצבעים). השקפים שלה
נמחקים, ושקפים חדשים נבנים על אותן פריסות. כל מספר בשקפים נקרא מקובץ CSV
ב-week 6/output, כך שאחרי הרצה מחדש של קוד ה-R מספיק להריץ גם את הקובץ הזה.

הרצה מתיקיית השורש:  python3 "week 6/build_slides.py"
דרישות: python-pptx, lxml.
"""
import csv
import os

from lxml import etree
from pptx import Presentation
from pptx.chart.data import CategoryChartData, XyChartData
from pptx.dml.color import RGBColor
from pptx.enum.chart import XL_CHART_TYPE, XL_LABEL_POSITION, XL_LEGEND_POSITION, XL_MARKER_STYLE
from pptx.enum.dml import MSO_LINE_DASH_STYLE
from pptx.enum.shapes import MSO_SHAPE
from pptx.enum.text import MSO_ANCHOR, MSO_AUTO_SIZE
from pptx.oxml.ns import qn
from pptx.util import Inches, Pt

TEMPLATE = os.path.join("week 5", "BPD_week5_presentation.pptx")
OUT = os.path.join("week 6", "BPD_week6_presentation.pptx")
OUTD = os.path.join("week 6", "output")

# ---- צבעים: כמו בשבועות 3–5 ----
NAVY, TEXT, MUTED = "14303F", "4F606C", "7A8C99"
GOLD_BG, GOLD_LN, GOLD_TX = "FBF1DF", "B8862E", "7A5212"
CARD, BORDER, GRID, AXIS = "EEF3F6", "D5DDE3", "E3E8EC", "B9C3CA"
ROW_ALT, ROW_HI = "F5F8FA", "FBE8DD"
TRACK_OFF_TX = "5E707D"
# משמעות קבועה: כתום = נורמלי (חלון יומי נורמלי הוא השיטה שנבחרה),
# אפור-כחלחל = אמפירי, כחול = מודל קיים (הספליין של שבוע 5), אפור = היעד.
# בגרפי קו: קו מלא = חלון יומי, מקווקו = שבוע שלם. בעמודות: גוון בהיר = שבוע שלם.
ORANGE, ORANGE_LT = "C2510F", "E3A07A"
ALT, ALT_LT = "4F606C", "A9B5BE"
BLUE, TARGET = "0B6AA8", "8C969E"
HIGHLIGHT_BAR = NAVY

PCTS = [3, 5, 10, 95, 97.5]
TRACKER = ["1. שבועי", "2. דיוק", "3. יומי", "4. השוואה"]


# ======================== נתונים ========================
def rd(name):
    with open(os.path.join(OUTD, name), encoding="utf-8") as f:
        return list(csv.DictReader(f))


def plab(p):
    return "P" + ("%g" % p)


data_check = {r["check"]: r["value"] for r in rd("00_data_check.csv")}
weekly_sizes = rd("01_weekly_group_sizes.csv")
window_sizes = rd("01_daily_window_group_sizes.csv")
w_normal = rd("02_weekly_normal.csv")
w_emp = rd("03_weekly_empirical.csv")
ms = {(r["method"], r["evaluation"], float(r["percentile"])): r for r in rd("08_methods_summary.csv")}
pen = {(r["method"], r["evaluation"]): r for r in rd("08_penalty.csv")}
dw = rd("08_coverage_by_day_in_week.csv")
dws = {(r["method"], r["evaluation"], float(r["percentile"])): r for r in rd("08_day_in_week_summary.csv")}
wties = {float(r["percentile"]): r for r in rd("05_weekly_ties_summary.csv")}
dties = {float(r["percentile"]): r for r in rd("07_daily_ties_summary.csv")}
tie_ex = rd("05_tie_example.csv")


def num(r, k):
    return float(r[k])


def pct(v, d=2):
    return f"{float(v):.{d}f}%"


def cov(method, p, ev="cv", k="below_pct"):
    return num(ms[(method, ev, float(p))], k)


def dfirst(method, p, ev="cv"):
    r = dws[(method, ev, float(p))]
    return num(r, "first_day_pct"), num(r, "last_day_pct")


def penalty(method, ev="cv"):
    return num(pen[(method, ev)], "penalty")


# מספרים שחוזרים בכמה שקפים
N_MEAS = int(data_check["measurements"])
N_PREG = int(data_check["pregnancies"])
WHOLE_MM = data_check["bpd_whole_mm_pct"]
DIFF = [num(r, "empirical_minus_normal") for r in w_emp]
MAX_ABS_DIFF = max(abs(min(DIFF)), max(DIFF))
WIN_N = [int(r["n"]) for r in window_sizes]
EXACT_W = sum(int(r["groups_exact_possible"]) for r in wties.values())
GROUPS_W = sum(int(r["n_groups"]) for r in wties.values())
EXACT_D = sum(int(r["groups_exact_possible"]) for r in dties.values())
GROUPS_D = sum(int(r["n_groups"]) for r in dties.values())
MED_GAP = [num(r, "median_best_gap_pp") for r in wties.values()]
MAX_GAP = max(num(r, "max_best_gap_pp") for r in wties.values())
TIE_WEEK = int(tie_ex[0]["week"])
TIE_P = float(tie_ex[0]["percentile"])
TIE_ROW = max(tie_ex, key=lambda r: num(r, "n") if num(r, "below_pct") < TIE_P <= num(r, "at_or_below_pct") else -1)
TIE_VAL = int(float(TIE_ROW["bpd"]))
WN_FIRST, WN_LAST = dfirst("week_normal", 10)
DN_FIRST, DN_LAST = dfirst("day_normal", 10)
DAY280 = int(data_check["measurements_day_280"])


# ======================== כלי עזר לטקסט ========================
def T(text, size=16, bold=False, color=TEXT):
    return (text, size, bold, color)


def style_run(r, size, bold=False, color=TEXT, font="Assistant"):
    f = r.font
    f.size = Pt(size)
    f.bold = bold
    f.color.rgb = RGBColor.from_string(color)
    rPr = r._r.get_or_add_rPr()
    rPr.set("lang", "he-IL")
    for tag in ("a:latin", "a:cs"):
        el = rPr.find(qn(tag))
        if el is None:
            el = etree.SubElement(rPr, qn(tag))
        el.set("typeface", font)


def has_hebrew(s):
    return any("֐" <= ch <= "׿" for ch in str(s))


def add_para(tf, runs, first, align="r", rtl=True, after=4):
    p = tf.paragraphs[0] if first else tf.add_paragraph()
    pPr = p._p.get_or_add_pPr()
    pPr.set("algn", align)
    pPr.set("rtl", "1" if rtl else "0")
    p.space_after = Pt(after)
    for text, size, bold, color in runs:
        r = p.add_run()
        r.text = text
        style_run(r, size, bold, color)
    return p


def fill_tf(tf, paras, after=4):
    """paras: רשימה של פסקאות. פסקה = רשימת runs, או dict עם runs/align/after."""
    for i, pr in enumerate(paras):
        if isinstance(pr, dict):
            add_para(tf, pr["runs"], i == 0, pr.get("align", "r"), pr.get("rtl", True), pr.get("after", after))
        else:
            add_para(tf, pr, i == 0, after=after)


def set_insets(tf, lr=0.05, tb=0.03):
    tf.margin_left = tf.margin_right = Inches(lr)
    tf.margin_top = tf.margin_bottom = Inches(tb)


def textbox(slide, x, y, w, h, paras, anchor=MSO_ANCHOR.TOP, after=4, name=None):
    tb = slide.shapes.add_textbox(Inches(x), Inches(y), Inches(w), Inches(h))
    if name:
        tb.name = name
    tf = tb.text_frame
    tf.word_wrap = True
    tf.auto_size = MSO_AUTO_SIZE.NONE
    tf.vertical_anchor = anchor
    set_insets(tf)
    fill_tf(tf, paras, after)
    return tb


def box(slide, x, y, w, h, fill, line=None, paras=None, inset=0.15, anchor=MSO_ANCHOR.TOP,
        after=4, shape=MSO_SHAPE.ROUNDED_RECTANGLE, name=None):
    shp = slide.shapes.add_shape(shape, Inches(x), Inches(y), Inches(w), Inches(h))
    if name:
        shp.name = name
    if shape == MSO_SHAPE.ROUNDED_RECTANGLE:
        shp.adjustments[0] = 0.08
    shp.fill.solid()
    shp.fill.fore_color.rgb = RGBColor.from_string(fill)
    if line:
        shp.line.color.rgb = RGBColor.from_string(line)
        shp.line.width = Pt(1.25)
    else:
        shp.line.fill.background()
    etree.SubElement(shp._element.spPr, qn("a:effectLst"))     # בלי צל
    tf = shp.text_frame
    tf.word_wrap = True
    tf.auto_size = MSO_AUTO_SIZE.NONE
    tf.vertical_anchor = anchor
    set_insets(tf, inset, inset * 0.6)
    if paras:
        fill_tf(tf, paras, after)
    return shp


def decision(slide, x, y, w, h, question, items):
    """מלבן החלטה בגוון זהב, כמו בשבועות 4–5: השאלה, בחרנו, למה, המחיר."""
    paras = [[T(question, 18, True, GOLD_TX)]]
    paras += [[T(k + ": ", 16, True, NAVY), T(v, 16, False, TEXT)] for k, v in items]
    return box(slide, x, y, w, h, GOLD_BG, GOLD_LN, paras, after=5, name="Decision")


def card(slide, x, y, w, h, title, body, size=16):
    paras = [[T(title, 18, True, NAVY)]]
    bodies = body if isinstance(body, list) else [body]
    paras += [[T(b, size, False, TEXT)] for b in bodies]
    return box(slide, x, y, w, h, CARD, None, paras, name="Card")


def caption(slide, x, y, w, text, h=0.3, align="r", size=12.5):
    return textbox(slide, x, y, w, h, [{"runs": [T(text, size, False, MUTED)], "align": align}])


# ======================== כלי עזר לשקף ========================
def page_number(slide, n):
    tb = slide.shapes.add_textbox(Inches(0.6), Inches(7.05), Inches(0.6), Inches(0.3))
    tb.name = "Page number"
    tf = tb.text_frame
    set_insets(tf)
    p = tf.paragraphs[0]
    pPr = p._p.get_or_add_pPr()
    pPr.set("algn", "l")
    pPr.set("rtl", "1")
    fld = etree.SubElement(p._p, qn("a:fld"))
    fld.set("id", "{B6F15528-21DE-4FAA-801E-634DDDAF4B2B}")
    fld.set("type", "slidenum")
    rPr = etree.SubElement(fld, qn("a:rPr"))
    rPr.set("lang", "he-IL")
    rPr.set("sz", "1000")
    sf = etree.SubElement(rPr, qn("a:solidFill"))
    etree.SubElement(sf, qn("a:srgbClr")).set("val", MUTED)
    etree.SubElement(rPr, qn("a:latin")).set("typeface", "Assistant")
    etree.SubElement(rPr, qn("a:cs")).set("typeface", "Assistant")
    etree.SubElement(fld, qn("a:t")).text = str(n)


def set_title(slide, text, size=28):
    # בתבנית הכותרת היא placeholder מסוג title עם idx=100, ולכן shapes.title לא מוצא אותה
    ph = next(sh for sh in slide.placeholders if "TITLE" in str(sh.placeholder_format.type))
    tf = ph.text_frame
    tf.text = ""
    p = tf.paragraphs[0]
    p._p.get_or_add_pPr().set("rtl", "1")
    r = p.add_run()
    r.text = text
    r.font.size = Pt(size)
    rPr = r._r.get_or_add_rPr()
    rPr.set("lang", "he-IL")
    etree.SubElement(rPr, qn("a:latin")).set("typeface", "Assistant")
    etree.SubElement(rPr, qn("a:cs")).set("typeface", "Assistant")


def tracker(slide, active):
    xs = [4.51, 3.21, 1.90, 0.60]
    for i, (label, x) in enumerate(zip(TRACKER, xs)):
        on = (i + 1) == active
        shp = box(slide, x, 0.49, 1.22, 0.33, NAVY if on else CARD, None,
                  [{"runs": [T(label, 14, on, "FFFFFF" if on else TRACK_OFF_TX)], "align": "ctr", "after": 0}],
                  inset=0, anchor=MSO_ANCHOR.MIDDLE, name=f"Tracker {i + 1}")
        shp.adjustments[0] = 0.5


def content_slide(prs, title, subtitle, n, active=None, notes=None, hidden=False):
    s = prs.slides.add_slide(prs.slide_layouts[1])          # CONTENT
    set_title(s, title)
    if subtitle:
        textbox(s, 0.6, 1.06, 12.13, 0.36, [[T(subtitle, 16, False, TEXT)]], name="Conclusion")
    if active:
        tracker(s, active)
    page_number(s, n)
    if notes:
        s.notes_slide.notes_text_frame.text = notes
    if hidden:
        s._element.set("show", "0")
    return s


# ======================== טבלאות ========================
def _ln(parent, tag):
    ln = etree.SubElement(parent, qn(tag))
    ln.set("w", "6350")
    sf = etree.SubElement(ln, qn("a:solidFill"))
    etree.SubElement(sf, qn("a:srgbClr")).set("val", BORDER)


def table(slide, x, y, col_w, rows, row_h=0.36, size=13, highlight=(), first_col_bold=True):
    """rows[0] = כותרת. העמודה הראשונה היא הימנית (טבלה מימין לשמאל).
    תא בלי עברית נכתב משמאל לימין, כדי שסוגריים וסימנים לא יתהפכו."""
    nr, nc = len(rows), len(rows[0])
    gf = slide.shapes.add_table(nr, nc, Inches(x), Inches(y), Inches(sum(col_w)), Inches(row_h * nr))
    tbl = gf.table
    tblPr = tbl._tbl.tblPr
    tblPr.set("rtl", "1")
    tblPr.set("bandRow", "0")
    for j, w in enumerate(col_w):
        tbl.columns[j].width = Inches(w)
    for i in range(nr):
        tbl.rows[i].height = Inches(row_h)
    for i, row in enumerate(rows):
        for j, val in enumerate(row):
            cell = tbl.cell(i, j)
            head, hi = i == 0, i in highlight
            heb = has_hebrew(val)
            if head:
                color, bold, fill = "FFFFFF", True, NAVY
            else:
                fill = ROW_HI if hi else ("FFFFFF" if i % 2 else ROW_ALT)
                color = ORANGE if (hi and j == 0) else (NAVY if j == 0 else TEXT)
                bold = (j == 0 and first_col_bold) or hi
            align = "r" if (heb and j == 0 and not head) else "ctr"
            tf = cell.text_frame
            tf.word_wrap = True
            add_para(tf, [T(str(val), size, bold, color)], True, align=align, rtl=heb, after=0)
            tcPr = cell._tc.get_or_add_tcPr()
            for k in list(tcPr):
                tcPr.remove(k)
            for a, v in (("marL", "54864"), ("marR", "54864"), ("marT", "27432"), ("marB", "27432"), ("anchor", "ctr")):
                tcPr.set(a, v)
            for tag in ("a:lnL", "a:lnR", "a:lnT", "a:lnB"):
                _ln(tcPr, tag)
            sf = etree.SubElement(tcPr, qn("a:solidFill"))
            etree.SubElement(sf, qn("a:srgbClr")).set("val", fill)
    return gf


# ======================== גרפים ========================
def font_cs(font, size, color=TEXT, bold=False):
    font.size = Pt(size)
    font.bold = bold
    font.name = "Assistant"
    font.color.rgb = RGBColor.from_string(color)
    rPr = font._rPr
    if rPr.find(qn("a:cs")) is None:
        etree.SubElement(rPr, qn("a:cs")).set("typeface", "Assistant")


def rich_text(tf, text, size, color, bold=False):
    tf.text = ""
    add_para(tf, [T(text, size, bold, color)], True, align="ctr", after=0)


def style_axes(ch, x_title=None, y_title=None, xmin=None, xmax=None, xunit=None,
               ymin=None, ymax=None, yunit=None, xfmt="0", yfmt="0"):
    xa, ya = ch.category_axis, ch.value_axis
    for ax, title, lo, hi, unit, fmt in ((xa, x_title, xmin, xmax, xunit, xfmt), (ya, y_title, ymin, ymax, yunit, yfmt)):
        font_cs(ax.tick_labels.font, 13.5)
        if fmt and hasattr(ax, "minimum_scale"):
            ax.tick_labels.number_format = fmt
            ax.tick_labels.number_format_is_linked = False
        if lo is not None:
            ax.minimum_scale = lo
        if hi is not None:
            ax.maximum_scale = hi
        if unit is not None:
            ax.major_unit = unit
        if title:
            ax.has_title = True
            rich_text(ax.axis_title.text_frame, title, 13.5, TEXT)
    xa.has_major_gridlines = False
    xa.format.line.color.rgb = RGBColor.from_string(AXIS)
    ya.has_major_gridlines = True
    ya.major_gridlines.format.line.color.rgb = RGBColor.from_string(GRID)
    ya.format.line.fill.background()


def chart_title(ch, text):
    ch.has_title = True
    rich_text(ch.chart_title.text_frame, text, 16, NAVY)


def legend(ch, hide=()):
    ch.has_legend = True
    ch.legend.position = XL_LEGEND_POSITION.BOTTOM
    ch.legend.include_in_layout = False
    font_cs(ch.legend.font, 13.5)
    pos = ch.legend._element.find(qn("c:legendPos"))
    for idx in sorted(hide, reverse=True):
        le = etree.Element(qn("c:legendEntry"))
        etree.SubElement(le, qn("c:idx")).set("val", str(idx))
        etree.SubElement(le, qn("c:delete")).set("val", "1")
        pos.addnext(le)


def manual_legend(slide, y, items, right):
    """מקרא מצורות, מימין לשמאל: קו או נקודה בצבע, ולידם התווית."""
    from pptx.enum.shapes import MSO_CONNECTOR
    x = right
    for kind, color, label in items:
        x -= 0.45
        if kind == "line":
            ln = slide.shapes.add_connector(MSO_CONNECTOR.STRAIGHT, Inches(x), Inches(y + 0.15),
                                            Inches(x + 0.35), Inches(y + 0.15))
            ln.line.color.rgb = RGBColor.from_string(color)
            ln.line.width = Pt(2.25)
        else:
            box(slide, x + 0.11, y + 0.08, 0.14, 0.14, color, None, shape=MSO_SHAPE.OVAL)
        x -= 1.1
        textbox(slide, x, y, 1.05, 0.3, [{"runs": [T(label, 13.5, False, TEXT)], "align": "r", "after": 0}])
        x -= 0.25


def xy_chart(slide, x, y, w, h, series):
    """series: dict(name, xs, ys, color, width, dash, marker, line=True)"""
    cd = XyChartData()
    for s in series:
        ser = cd.add_series(s["name"])
        for a, b in zip(s["xs"], s["ys"]):
            ser.add_data_point(a, b)
    gf = slide.shapes.add_chart(XL_CHART_TYPE.XY_SCATTER_LINES_NO_MARKERS,
                                Inches(x), Inches(y), Inches(w), Inches(h), cd)
    ch = gf.chart
    for s, ser in zip(series, ch.plots[0].series):
        ser.smooth = False
        ln = ser.format.line
        if s.get("line", True):
            ln.color.rgb = RGBColor.from_string(s["color"])
            ln.width = Pt(s.get("width", 2.25))
            if s.get("dash"):
                ln.dash_style = MSO_LINE_DASH_STYLE.DASH
        else:
            ln.fill.background()
        if s.get("marker"):
            ser.marker.style = XL_MARKER_STYLE.CIRCLE
            ser.marker.size = s.get("marker_size", 6)
            ser.marker.format.fill.solid()
            ser.marker.format.fill.fore_color.rgb = RGBColor.from_string(s["color"])
            ser.marker.format.line.color.rgb = RGBColor.from_string(s["color"])
        else:
            ser.marker.style = XL_MARKER_STYLE.NONE
    return ch


def bar_chart(slide, x, y, w, h, cats, values, colors, horizontal=False, fmt="0.0", gap=60, label_size=13.5):
    cd = CategoryChartData()
    cd.categories = cats
    cd.add_series("s", values)
    kind = XL_CHART_TYPE.BAR_CLUSTERED if horizontal else XL_CHART_TYPE.COLUMN_CLUSTERED
    gf = slide.shapes.add_chart(kind, Inches(x), Inches(y), Inches(w), Inches(h), cd)
    ch = gf.chart
    plot = ch.plots[0]
    plot.gap_width = gap
    plot.vary_by_categories = False
    ser = plot.series[0]
    for i, c in enumerate(colors):
        pt = ser.points[i]
        pt.format.fill.solid()
        pt.format.fill.fore_color.rgb = RGBColor.from_string(c)
        pt.format.line.fill.background()
    plot.has_data_labels = True
    dl = plot.data_labels
    dl.number_format = fmt
    dl.number_format_is_linked = False
    dl.position = XL_LABEL_POSITION.OUTSIDE_END
    font_cs(dl.font, label_size, NAVY)
    ch.has_legend = False
    return ch


# ======================== בניית המצגת ========================
prs = Presentation(TEMPLATE)
# מוחקים את שקפי שבוע 5, וגם את רשימת המקטעים שמפנה אליהם
sld_ids = prs.slides._sldIdLst
for sld in list(sld_ids):
    prs.part.drop_rel(sld.rId)
    sld_ids.remove(sld)
for ext in prs.part._element.findall(".//" + qn("p:extLst") + "/" + qn("p:ext")):
    if ext.find("{http://schemas.microsoft.com/office/powerpoint/2010/main}sectionLst") is not None:
        ext.getparent().remove(ext)
prs.core_properties.title = "BPD week 6"

# ---------- 1. פתיחה ----------
s = prs.slides.add_slide(prs.slide_layouts[2])               # DARK
set_title(s, "אחוזוני BPD לכל שבוע ובחלון יומי", size=44)
textbox(s, 0.6, 4.0, 12.13, 0.5,
        [[T("אחוזונים נורמליים ואמפיריים בלי נוסחה: עד כמה אפשר לדייק, והאם חלוקה לשבועות מספיקה", 22, False, "C9DCE8")]])
textbox(s, 0.6, 6.3, 12.13, 0.3,
        [[T("קבוצת BPD5  |  מעבדה סטטיסטית  |  שבוע 6  |  נובמבר 2026", 14, False, "9FB6C6")]])

# ---------- 2. השאלה ----------
s = content_slide(prs, "השאלה: האם מספיקה טבלה לכל שבוע?",
                  "גיל ההריון לפי הווסת אינו מדויק ברמת היום. לכן בודקים אחוזונים שמחושבים לכל שבוע בנפרד, בלי נוסחה.",
                  2, notes=(
                      "עד עכשיו בנינו עקומה רציפה לפי גיל מדויק בימים (שבועות 4 ו-5). המטלה הפעם שואלת אם אפשר בלי זה: "
                      "הגיל לפי הווסת לא מדויק, כי הביוץ לא תמיד באותו מרחק מהווסת והתאריך נרשם מהזיכרון. "
                      "לכן מחשבים לכל שבוע טבלה משלו. ארבעה שלבים: שתי שיטות לחישוב שבועי, עד כמה אפשר לדייק, "
                      "חלון יומי, והשוואה בין שבועי ליומי. 93.1% מהמדידות הן מילימטרים שלמים, וזה יהיה חשוב בשלב 2."))
steps = ["1. שבועי: נורמלי ואמפירי", "2. עד כמה אפשר לדייק", "3. חלון יומי של 7 ימים", "4. שבועי מול יומי"]
xs = [9.88, 6.79, 3.70, 0.61]
for i, (label, x) in enumerate(zip(steps, xs)):
    box(s, x, 1.95, 2.85, 0.62, CARD, None, [{"runs": [T(label, 16, True, NAVY)], "align": "ctr", "after": 0}],
        anchor=MSO_ANCHOR.MIDDLE, inset=0.05, name=f"Step {i + 1}")
    if i < 3:
        textbox(s, x - 0.24, 1.95, 0.24, 0.62, [{"runs": [T("←", 16, True, MUTED)], "align": "ctr", "after": 0}],
                anchor=MSO_ANCHOR.MIDDLE)
card(s, 6.85, 2.95, 5.88, 1.85, "למה לא לפי יום?",
     ["הביוץ וההפריה לא תמיד באותו מרחק מהווסת.",
      "תאריך הווסת נרשם לפי מה שהאישה זוכרת."])
card(s, 6.85, 5.0, 5.88, 1.85, "מה במקום נוסחה?",
     ["לכל שבוע בנפרד: ממוצע ושונות, או סידור המדידות מהקטנה לגדולה. בלי מודל רציף כמו בשבועות 4-5."])
stats = [(f"{N_MEAS:,}", "מדידות בקובץ האימון"), (f"{N_PREG:,}", "הריונות"),
         ("26", "שבועות, 14 עד 39"), (f"{WHOLE_MM}%", "מהמדידות במילימטרים שלמים")]
for i, (big, lab) in enumerate(stats):
    cx = 3.6 if i % 2 == 0 else 0.6
    cy = 2.95 if i < 2 else 5.0
    box(s, cx, cy, 2.85, 1.85, "FFFFFF", BORDER,
        [{"runs": [T(big, 36, True, ORANGE if i == 3 else NAVY)], "align": "ctr", "after": 2},
         {"runs": [T(lab, 15, False, TEXT)], "align": "ctr", "after": 0}],
        anchor=MSO_ANCHOR.MIDDLE, name="Stat")

# ---------- 3. שתי השיטות ----------
s = content_slide(prs, "שתי דרכים לחשב אחוזון בכל שבוע",
                  f"שני הספים כמעט זהים: בכל שבוע ובכל אחוזון ההפרש ביניהם עד {MAX_ABS_DIFF:.2f} מ\"מ.",
                  3, active=1, notes=(
                      "הנורמלי: מחשבים את הממוצע ואת סטיית התקן של מדידות השבוע, והסף הוא ממוצע ועוד z כפול סטיית התקן. "
                      "למשל P10 נמצא 1.28 סטיות תקן מתחת לממוצע. האמפירי: מסדרים את המדידות, ולוקחים את הערך הקטן ביותר "
                      "שלפחות P אחוז מהמדידות קטנות ממנו או שוות לו. ב-R זה quantile עם type 1. "
                      "בגרף רואים שהנקודות של האמפירי יושבות כמעט על הקווים של הנורמלי. "
                      "השתמשנו באותם חמישה אחוזונים כמו בשבועות 4-5, כדי שנוכל להשוות לשבוע 5."))
# סדרה לכל אחוזון ולכל שיטה. המקרא של הגרף כבוי, ובמקומו מקרא מצורות מתחת לגרף,
# כדי שיופיעו בו רק שתי שורות בכל תוכנה שפותחת את הקובץ.
series = []
for p in PCTS:
    rows = [r for r in w_normal if float(r["percentile"]) == p]
    series.append(dict(name=f"נורמלי {plab(p)}", xs=[int(r["week"]) for r in rows],
                       ys=[num(r, "normal") for r in rows], color=ORANGE, width=2.25))
for p in PCTS:
    rows = [r for r in w_emp if float(r["percentile"]) == p]
    series.append(dict(name=f"אמפירי {plab(p)}", xs=[int(r["week"]) for r in rows],
                       ys=[num(r, "empirical") for r in rows], color=ALT, line=False, marker=True, marker_size=5))
ch = xy_chart(s, 0.6, 1.95, 7.3, 4.55, series)
chart_title(ch, "האחוזונים P3, P5, P10, P95 ו-P97.5 בכל שבוע, במ\"מ")
style_axes(ch, "שבוע הריון", "BPD במ\"מ", 14, 40, 2, 20, 100, 20)
ch.has_legend = False
manual_legend(s, 6.62, [("line", ORANGE, "נורמלי"), ("dot", ALT, "אמפירי")], right=7.9)
card(s, 8.15, 1.95, 4.58, 1.45, "נורמלי",
     "הממוצע של השבוע, ועוד z סטיות תקן שלו. הסף הוא מספר עם שברים.")
card(s, 8.15, 3.55, 4.58, 1.45, "אמפירי",
     "הערך הקטן ביותר שלפחות P% מהמדידות קטנות ממנו או שוות לו.")
decision(s, 8.15, 5.15, 4.58, 1.75, "איזו הגדרה לאמפירי?", [
    ("בחרנו", "ערך שנמדד בפועל"),
    ("המחיר", "מתחת לסף יש תמיד פחות מהיעד, או בדיוק היעד")])

# ---------- 4. השוואת השיטות השבועיות ----------
p10n, p10e, p10ea = cov("week_normal", 10), cov("week_empirical", 10), cov("week_empirical", 10, k="at_or_below_pct")
s = content_slide(prs, "הנורמלי קרוב ליעד, האמפירי מפספס",
                  "באמפירי היעד נמצא תמיד בין \"מתחת לסף\" ל\"עד הסף, כולל\": הבעיה היא מדידות ששוות בדיוק לסף.",
                  4, active=1, notes=(
                      "זו ההשוואה שהמטלה ביקשה: איזה אחוז מהמדידות נמצא מתחת לכל סף. "
                      f"בנורמלי המספרים קרובים ליעד, למשל {pct(p10n)} ב-P10. באמפירי {pct(p10e)} מתחת לסף, "
                      f"ו-{pct(p10ea)} אם סופרים גם את המדידות ששוות לו. כלומר הסף במקום הנכון, אבל על הערך הזה יושבות הרבה מדידות. "
                      "בדקנו על הריונות מוסתרים: cross-validation לפי הריון, 5 קבוצות, אותן קבוצות כמו בשבועות 4-5. "
                      "הסיבה: הסף האמפירי בנוי בדיוק מהמדידות האלה, ולכן בדיקה עליהן כמעט מעגלית. "
                      "על כל הנתונים המספרים כמעט זהים, כי בכל שבוע יש מאות עד אלפי מדידות."))
rows = [["אחוזון", "יעד", "נורמלי: מתחת", "אמפירי: מתחת", "אמפירי: עד הסף, כולל"]]
for p in PCTS:
    rows.append([plab(p), f"{p:g}%", pct(cov("week_normal", p)), pct(cov("week_empirical", p)),
                 pct(cov("week_empirical", p, k="at_or_below_pct"))])
textbox(s, 0.6, 1.92, 7.5, 0.33, [[T("אחוז המדידות מתחת לסף, על הריונות מוסתרים", 16, True, NAVY)]])
table(s, 0.6, 2.3, [1.1, 1.0, 1.75, 1.75, 1.9], rows, row_h=0.45, size=14)
ins_n = cov("week_normal", 10, "in_sample")
ins_e = cov("week_empirical", 10, "in_sample")
ins_ea = cov("week_empirical", 10, "in_sample", "at_or_below_pct")
card(s, 0.6, 5.3, 7.5, 1.6, "ועל כל הנתונים?",
     f"כמעט אותם מספרים. ב-P10: נורמלי {pct(ins_n)}, אמפירי {pct(ins_e)} עד {pct(ins_ea)}.")
decision(s, 8.35, 1.95, 4.38, 3.2, "על אילו נתונים בודקים?", [
    ("בחרנו", "הריונות מוסתרים: 5 קבוצות, מחשבים על 4 ובודקים על החמישית"),
    ("למה", "הסף האמפירי בנוי מאותן מדידות. בדיקה עליהן מעגלית"),
    ("המחיר", "כל סף מחושב מ-80% מההריונות")])
card(s, 8.35, 5.3, 4.38, 1.6, "מה נחשב \"מתחת\"?",
     "רק מדידה קטנה מהסף, כלשון המטלה. מדידה ששווה לסף מדווחת לחוד.")

# ---------- 5. ערכים חוזרים ----------
below = num(TIE_ROW, "below_pct")
atbelow = num(TIE_ROW, "at_or_below_pct")
s = content_slide(prs, "אי אפשר לפגוע בדיוק באחוזון",
                  f"{WHOLE_MM}% מהמדידות הן מילימטרים שלמים. כשערך אחד חוזר עשרות פעמים, הסף נמצא מתחת לכולן או מעל כולן.",
                  5, active=2, notes=(
                      f"הדוגמה היא המקרה הגרוע ביותר: שבוע {TIE_WEEK}, P10. האחוזון נופל על הערך {TIE_VAL} מ\"מ, "
                      f"ו-{int(TIE_ROW['n'])} מדידות שוות לו. אם הסף ממש מתחת ל-{TIE_VAL}, יש {pct(below)} מתחתיו. "
                      f"אם ממש מעליו, {pct(atbelow)}. אין שום סף באמצע, ולכן הכי קרוב ל-10% שאפשר להגיע הוא {MAX_GAP:.2f} נקודות אחוז. "
                      "בטבלה: בשאר השבועות זה פחות קיצוני, אבל בחציון עדיין שליש נקודה עד נקודה שלמה. "
                      "בלי ערכים חוזרים הפער היה כמעט אפס, רק בגלל ש-n כפול P לא תמיד מספר שלם. "
                      f"בדיוק אפשר רק ב-{EXACT_W} מתוך {GROUPS_W} צירופים של שבוע ואחוזון."))
cats = [str(int(float(r["bpd"]))) for r in tie_ex]
vals = [num(r, "pct") for r in tie_ex]
cols = [HIGHLIGHT_BAR if int(float(r["bpd"])) == TIE_VAL else ALT_LT for r in tie_ex]
ch = bar_chart(s, 0.6, 1.95, 6.2, 3.3, cats, vals, cols, fmt='0.0', gap=40, label_size=12)
chart_title(ch, f"שבוע {TIE_WEEK}: אחוז המדידות בכל ערך BPD")
style_axes(ch, "BPD במ\"מ", "אחוז", ymin=0, ymax=14, yunit=2, xfmt=None, yfmt='0')
for i, (big, lab, colr) in enumerate([(pct(below), f"מתחת ל-{TIE_VAL}", ALT),
                                      (f"{TIE_P:g}%", "היעד", TARGET),
                                      (pct(atbelow), f"עד {TIE_VAL}, כולל", ALT)]):
    box(s, 4.85 - i * 2.125, 5.4, 1.95, 1.5, "FFFFFF", BORDER,
        [{"runs": [T(big, 28, True, colr)], "align": "ctr", "after": 0},
         {"runs": [T(lab, 15, False, TEXT)], "align": "ctr", "after": 0}],
        anchor=MSO_ANCHOR.MIDDLE, name="Stat")
rows = [["אחוזון", "שווים לסף", "הכי קרוב", "הגרוע", "בלי חזרות"]]
for p in PCTS:
    r = wties[float(p)]
    rows.append([plab(p), pct(num(r, "median_tie_block_pct")), f"{num(r, 'median_best_gap_pp'):.2f}",
                 f"{num(r, 'max_best_gap_pp'):.2f}", f"{num(r, 'median_gap_without_ties_pp'):.2f}"])
textbox(s, 7.05, 1.92, 5.68, 0.33, [[T("הפער הקטן ביותר מהיעד שסף כלשהו משיג", 16, True, NAVY)]])
table(s, 7.05, 2.3, [0.95, 1.2, 1.15, 1.13, 1.25], rows, row_h=0.4, size=13.5)
caption(s, 7.05, 4.73, 5.68, f"חציון על 26 השבועות, בנקודות אחוז. \"שווים לסף\": אחוז המדידות בערך שבו נופל האחוזון. בדיוק אפשר רק ב-{EXACT_W} מתוך {GROUPS_W} צירופים של שבוע ואחוזון.", h=0.55)
decision(s, 7.05, 5.3, 5.68, 1.6, "איך עונים על \"אפשר בדיוק?\"", [
    ("בחרנו", "האחוז הקרוב ביותר שסף כלשהו משיג"),
    ("למה", "תשובת כן/לא תלויה כמעט רק בשאלה אם n·P שלם")])

# ---------- 6. חלון יומי ----------
mid_tie_d = num(dties[10.0], "median_tie_block_pct")
s = content_slide(prs, "חלון יומי: הסף עולה יחד עם העובר",
                  "לכל יום: כל המדידות מ-3 ימים לפניו ועד 3 ימים אחריו, במשקל שווה. אותו חישוב, נורמלי ואמפירי.",
                  6, active=3, notes=(
                      "זה העידון שהמטלה הציעה: ממוצע נע, או kernel עם התפלגות אחידה. לכל יום לוקחים את המדידות של שבעה ימים "
                      "סביבו, ומחשבים מהן בדיוק את אותם שני ספים. החלון זז ביום אחד בכל פעם, ולכן הסף משתנה בהדרגה. "
                      "בגרף: הקו המקווקו הוא הסף השבועי, שקבוע לאורך כל השבוע וקופץ בין שבועות. הקו המלא הוא הסף היומי, "
                      "שעולה יחד עם המדידות. החלון באורך שבוע, ולכן הערכים החוזרים לא נעלמים. "
                      "בקצוות, ימים 98-100 ו-278-280, החלון חסר. חישבנו גם שם וסימנו."))
s.shapes.add_picture(os.path.join(OUTD, "13_daily_vs_weekly_zoom.png"), Inches(0.6), Inches(1.95), width=Inches(7.4))
caption(s, 0.6, 6.42, 7.4, "קו מקווקו: הסף השבועי. קו מלא: הסף היומי. שבועות 20 עד 24, שיטה נורמלית, חמשת האחוזונים.", h=0.45)
card(s, 8.25, 1.95, 4.48, 1.75, "איך זה עובד",
     "יום 160: מדידות מימים 157 עד 163. יום 161: מ-158 עד 164. כל יום מקבל סף משלו.")
card(s, 8.25, 3.85, 4.48, 1.4, "גודל החלון",
     f"{min(WIN_N):,} עד {max(WIN_N):,} מדידות. שבעה ימים, כמו שבוע.")
decision(s, 8.25, 5.4, 4.48, 1.5, "מה עושים בקצוות?", [
    ("בחרנו", "מחשבים גם שם ומסמנים"),
    ("למה", "צריך סף לכל יום"),
    ("המחיר", "6 ימים עם חלון חסר")])

# ---------- 7. ההטיה בתוך השבוע ----------
s = content_slide(prs, "השיטה השבועית מוטה בתוך השבוע",
                  f"ביום הראשון בשבוע {pct(WN_FIRST)} מהעוברים מתחת ל-P10, וביום האחרון רק {pct(WN_LAST)}. בחלון היומי: {pct(DN_FIRST)} ו-{pct(DN_LAST)}.",
                  7, active=4, notes=(
                      "זה הממצא המרכזי. בשיטה השבועית הסף קבוע לכל השבוע, אבל העובר גדל. ביום הראשון של השבוע העוברים קטנים "
                      "מהממוצע של השבוע, ולכן יותר מהם נופלים מתחת לסף. ביום האחרון הם גדולים מהממוצע, ופחות נופלים. "
                      f"ב-P10: {pct(WN_FIRST)} ביום הראשון, כמעט פי 1.5 מהיעד, ו-{pct(WN_LAST)} ביום האחרון. "
                      "המשמעות: בתחילת השבוע הסף השבועי מסמן יותר מדי עוברים כקטנים, ובסופו מפספס עוברים קטנים. "
                      "בחלון היומי הקו כמעט שטוח. בבדיקה הרגילה, לפי שבוע, זה לא נראה בכלל, כי תחילת השבוע וסופו מבטלים זה את זה. "
                      f"יום 280 הוא היום השמיני של שבוע 39, ולכן לא נכלל בבדיקה הזו ({DAY280} מדידות). הכול על הריונות מוסתרים."))
order = [("week_normal", ORANGE, True, "שבועי, נורמלי"), ("day_normal", ORANGE, False, "חלון יומי, נורמלי"),
         ("week_empirical", ALT, True, "שבועי, אמפירי"), ("day_empirical", ALT, False, "חלון יומי, אמפירי")]
series = [dict(name="היעד: 10%", xs=[0, 6], ys=[10, 10], color=TARGET, width=1.25, dash=True)]
for m, c, dash, lab in order:
    rows_m = [r for r in dw if r["method"] == m and r["evaluation"] == "cv" and float(r["percentile"]) == 10]
    series.append(dict(name=lab, xs=[int(r["day_in_week"]) for r in rows_m], ys=[num(r, "below_pct") for r in rows_m],
                       color=c, dash=dash, marker=True, marker_size=5))
ch = xy_chart(s, 0.6, 1.95, 7.5, 4.6, series)
chart_title(ch, "אחוז המדידות מתחת ל-P10, לפי היום בתוך השבוע")
style_axes(ch, "היום בתוך השבוע: 0 = הראשון, 6 = האחרון", "אחוז", 0, 6, 1, 4, 16, 2)
legend(ch)
caption(s, 0.6, 6.55, 7.5, "על הריונות מוסתרים. קו מקווקו: שבוע שלם. קו מלא: חלון יומי.")
decision(s, 8.35, 1.95, 4.38, 2.25, "מספיק לבדוק לפי שבוע?", [
    ("בחרנו", "לבדוק גם לפי היום בתוך השבוע"),
    ("למה", "לפי שבוע, תחילת השבוע וסופו מבטלים זה את זה"),
    ("המחיר", f"יום 280 לא נכלל, {DAY280} מדידות")])
rows = [["אחוזון", "שבועי: ראשון", "שבועי: אחרון", "יומי: ראשון", "יומי: אחרון"]]
for p in PCTS:
    a, b = dfirst("week_normal", p)
    c, d = dfirst("day_normal", p)
    rows.append([plab(p), pct(a), pct(b), pct(c), pct(d)])
textbox(s, 8.35, 4.35, 4.38, 0.33, [[T("כל האחוזונים, שיטה נורמלית", 16, True, NAVY)]])
table(s, 8.35, 4.72, [0.78, 0.9, 0.9, 0.9, 0.9], rows, row_h=0.36, size=12.5)

# ---------- 8. קנס האחוזונים מול שבוע 5 ----------
pen_order = [("week_empirical", "שבועי, אמפירי", ALT_LT), ("day_empirical", "חלון יומי, אמפירי", ALT),
             ("week5_spline_reference", "הספליין של שבוע 5", BLUE), ("week_normal", "שבועי, נורמלי", ORANGE_LT),
             ("day_normal", "חלון יומי, נורמלי", ORANGE)]
pen_order.sort(key=lambda t: penalty(t[0]))
se_dn = num(pen[("day_normal", "cv")], "penalty_se")
s = content_slide(prs, "ארבע השיטות מול המודל של שבוע 5",
                  "לפי קנס האחוזונים, שלוש השיטות הנורמליות דומות. אבל הקנס נמדד לפי שבוע, ולכן אינו רואה את ההטיה בתוך השבוע.",
                  8, active=4, notes=(
                      "קנס האחוזונים הוא אותו מדד כמו בשבועות 4-5: לכל שבוע ואחוזון, המרחק בין אחוז המדידות מתחת לסף לבין היעד, "
                      "כשטעות באחוזון קיצוני נחשבת חמורה יותר. נמוך יותר זה טוב יותר. הכול על הריונות מוסתרים, ולכן אפשר להשוות "
                      f"ישירות לספליין של שבוע 5. החלון היומי הנורמלי הכי נמוך, {penalty('day_normal'):.4f}, מול {penalty('week5_spline_reference'):.4f} לספליין. "
                      f"טעות התקן כ-{se_dn:.3f}, כך שההבדלים בין השיטות הנורמליות קטנים משתי טעויות תקן. "
                      "השיטות האמפיריות רחוקות בבירור, בגלל הערכים החוזרים. "
                      "חשוב: הקנס בודק כל שבוע כיחידה אחת, ולכן השבועי הנורמלי נראה טוב כאן למרות ההטיה שראינו בשקף הקודם. "
                      "ההשוואה בין שבועי ליומי: כל מדידה מול הסף שחל עליה, של השבוע שלה או של היום שלה."))
ch = bar_chart(s, 0.6, 1.95, 7.5, 4.95, [t[1] for t in pen_order], [penalty(t[0]) for t in pen_order],
               [t[2] for t in pen_order], horizontal=True, fmt="0.0000", gap=50)
chart_title(ch, "קנס האחוזונים על הריונות מוסתרים. נמוך יותר = טוב יותר")
style_axes(ch, None, None, ymin=0, ymax=0.07, yunit=0.01, xfmt=None, yfmt="0.00")
card(s, 8.35, 1.95, 4.38, 1.75, "קנס האחוזונים",
     "הפער בין אחוז המדידות מתחת לסף לבין P, בכל שבוע ואחוזון. אותו מדד כמו בשבוע 5.")
decision(s, 8.35, 3.85, 4.38, 1.85, "איך משווים שבועי ליומי?", [
    ("בחרנו", "כל מדידה מול הסף שחל עליה"),
    ("למה", "כך כל מדידה נספרת פעם אחת")])
card(s, 8.35, 5.85, 4.38, 1.05, "טעות התקן",
     f"כ-{se_dn:.3f}. ההבדלים בין שלוש הנורמליות קטנים משתי טעויות תקן.")

# ---------- 9. מגבלות ----------
s = content_slide(prs, "מגבלות", None, 9, notes=(
    "ההסבר לחלון הרחב הוא השערה שלא בדקנו בקוד: בתוך שבעה ימים העובר גדל, ולכן הפיזור בחלון כולל גם את הגדילה. "
    "בשבועי זה לא פוגע בסיכום, כי המדידות שנבדקות פרושות על כל השבוע. ביומי המדידה תמיד במרכז החלון, ולכן התוספת "
    "מרחיבה את הסף שלא לצורך. תיקון אפשרי: להוריד את מגמת הגדילה בתוך החלון לפני חישוב הפיזור."))
lims = [
    ("החלון היומי מעט רחב מדי",
     f"{pct(cov('day_normal', 10))} מתחת ל-P10 ו-{pct(cov('day_normal', 95))} מתחת ל-P95. השערה שלא נבדקה: הגדילה בתוך 7 ימי החלון מוסיפה פיזור"),
    ("בקצוות החלון חסר", "בימים 98-100 ו-278-280 החלון חד-צדדי, ולכן הסף שם כנראה מוטה"),
    ("גיל ההריון לפי הווסת", "הבדיקה לפי היום בשבוע מניחה שהגיל ביום נכון. אם הוא שגוי בכמה ימים, ההטיה האמיתית קטנה יותר"),
    ("ערכים חוזרים בכל שיטה אמפירית", "מקורם במדידה במילימטרים שלמים, ולא בשיטה. גם החלון היומי לא פותר אותם"),
]
for i, (head, body) in enumerate(lims):
    yy = 1.48 + i * 1.18
    box(s, 12.18, yy, 0.55, 0.55, BLUE, None, [{"runs": [T(str(i + 1), 18, True, "FFFFFF")], "align": "ctr", "after": 0}],
        inset=0, anchor=MSO_ANCHOR.MIDDLE, shape=MSO_SHAPE.OVAL, name="Number")
    textbox(s, 2.6, yy - 0.05, 9.45, 0.9, [[T(head, 18, True, NAVY)], [T(body, 16, False, TEXT)]], after=2)
caption(s, 0.6, 6.6, 12.13, "כל הבדיקות נעשו על קובץ האימון בלבד. קובץ הבדיקה שמור להערכה בלתי תלויה.", size=14, align="ctr")

# ---------- 10. מסקנות ----------
s = content_slide(prs, "מסקנות: חלון יומי בהנחת נורמליות",
                  "החלוקה לשבועות פשוטה, אבל מוטה. החלון היומי מתקן את ההטיה, ועדיין נשאר טבלה בלי נוסחה.",
                  10, notes=(
                      "לסיכום, ארבעה ממצאים. הנורמלי עדיף על האמפירי בכל האחוזונים, והסיבה היא הערכים החוזרים. "
                      "אי אפשר לדייק באחוזון אמפירי. החלוקה לשבועות מוטה בתוך השבוע, ולכן היום כן נושא מידע, "
                      "גם אם הגיל לפי הווסת לא מושלם. והחלון היומי הנורמלי טוב לפחות כמו הספליין של שבוע 5."))
box(s, 0.6, 1.95, 12.13, 1.25, CARD, None,
    [[T("ההמלצה", 18, True, NAVY)],
     [T("לכל יום: ממוצע וסטיית תקן מהמדידות של 3 ימים לפניו ועד 3 ימים אחריו. האחוזון: הממוצע ועוד z סטיות תקן.", 16, False, TEXT)]],
    name="Recommendation")
cards = [
    ("הנורמלי עדיף על האמפירי", f"בכל האחוזונים. ב-P10 שבועי: {pct(p10n)} מול {pct(p10e)}."),
    ("אי אפשר לדייק באמפירי", f"הפער הקטן ביותר מהיעד: בחציון בין {min(MED_GAP):.2f} ל-{max(MED_GAP):.2f} נקודות אחוז, ועד {MAX_GAP:.2f} במקרה הגרוע."),
    ("החלוקה לשבועות מוטה", f"מתחת ל-P10: {pct(WN_FIRST)} ביום הראשון בשבוע, {pct(WN_LAST)} ביום האחרון."),
    ("החלון היומי לא נופל מהספליין", f"קנס {penalty('day_normal'):.4f} מול {penalty('week5_spline_reference'):.4f} בשבוע 5, ונשאר טבלה."),
]
for i, (head, body) in enumerate(cards):
    cx = 6.75 if i % 2 == 0 else 0.6
    cy = 3.4 if i < 2 else 5.2
    card(s, cx, cy, 5.98, 1.65, head, body)

# ======================== נספח מוסתר ========================
s = content_slide(prs, "נספח: כל ההחלטות", "כל החלטה מתועדת במלואה ב-README של שבוע 6, סעיף 7.", 11, hidden=True)
rows = [["ההחלטה", "בחרנו", "למה"],
        ["אילו אחוזונים", "3, 5, 10, 95, 97.5", "כמו בשבועות 4-5, כדי להשוות לקנס של שבוע 5"],
        ["אחוזון אמפירי", "ערך שנמדד בפועל, type 1", "רואים ישירות את בעיית הערכים החוזרים"],
        ["מה נחשב \"מתחת\"", "רק קטן מהסף. שווה לסף לחוד", "כלשון המטלה"],
        ["על אילו נתונים", "הריונות מוסתרים, 5 קבוצות", "בדיקה על אותן מדידות מעגלית"],
        ["אפשר בדיוק?", "האחוז הקרוב ביותר", "כן/לא תלוי כמעט רק ב-n·P"],
        ["קצוות החלון", "מחשבים ומסמנים", "צריך סף לכל יום. רק 6 ימים"],
        ["השוואת שבועי ליומי", "כל מדידה מול הסף שלה", "כל מדידה נספרת פעם אחת"],
        ["בדיקה נוספת", "לפי היום בתוך השבוע", "לפי שבוע ההטיה מתבטלת"],
        ["צבעים", "כתום נורמלי, אפור-כחלחל אמפירי", "כמו בשבועות 4-5"]]
table(s, 0.6, 1.95, [3.0, 4.2, 4.93], rows, row_h=0.47, size=14)

s = content_slide(prs, "נספח: ארבע השיטות בכל האחוזונים",
                  "אחוז המדידות מתחת לסף. באמפירי, אחרי הלוכסן: עד הסף, כולל.", 12, hidden=True)
for k, (ev, lab, yy) in enumerate([("cv", "על הריונות מוסתרים", 1.92), ("in_sample", "על כל הנתונים", 4.45)]):
    textbox(s, 0.6, yy, 12.13, 0.33, [[T(lab, 16, True, NAVY)]])
    rows = [["אחוזון", "יעד", "שבועי נורמלי", "שבועי אמפירי", "יומי נורמלי", "יומי אמפירי"]]
    for p in PCTS:
        rows.append([plab(p), f"{p:g}%", pct(cov("week_normal", p, ev)),
                     f"{pct(cov('week_empirical', p, ev))} / {pct(cov('week_empirical', p, ev, 'at_or_below_pct'))}",
                     pct(cov("day_normal", p, ev)),
                     f"{pct(cov('day_empirical', p, ev))} / {pct(cov('day_empirical', p, ev, 'at_or_below_pct'))}"])
    table(s, 0.6, yy + 0.38, [1.2, 1.1, 2.1, 3.0, 2.1, 2.63], rows, row_h=0.34, size=13)

for n, (title, sub, img) in enumerate([
        ("נספח: היום בתוך השבוע, כל האחוזונים", "על הריונות מוסתרים. כתום: נורמלי, אפור-כחלחל: אמפירי. מקווקו: שבוע שלם, מלא: חלון יומי.",
         "12_coverage_by_day_in_week.png"),
        ("נספח: ערכים חוזרים בכל שבוע", "על כל הנתונים. הקו האנכי האפור-כחלחל: המדידות ששוות לסף האמפירי.",
         "10_weekly_coverage_ties.png"),
        ("נספח: אחוז המדידות מתחת לסף בכל שבוע", "על הריונות מוסתרים, ארבע השיטות.", "11_cv_coverage_by_week.png")]):
    s = content_slide(prs, title, sub, 13 + n, hidden=True)
    s.shapes.add_picture(os.path.join(OUTD, img), Inches(2.45), Inches(1.6), height=Inches(5.3))

prs.save(OUT)
print("Saved", OUT, "|", len(prs.slides), "slides")
