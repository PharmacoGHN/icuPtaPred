[//]: # (Section for details changelog updates)
[//]: # (last updated on 2026-06-17)

<div style="font-family: inherit; max-width: 800px;">

  <div style="border-left: 4px solid #2c7be5; padding: 12px 16px; margin-bottom: 24px; background: #f0f6ff; border-radius: 0 6px 6px 0;">
    <h2 style="margin: 0 0 6px 0; font-size: 1.2em; color: #1a4f9e;">Version 1.0.0 — Initial Release
      <span style="font-weight: normal; font-size: 0.85em; color: #555; margin-left: 8px;">06/2026</span>
    </h2>
    <span style="display: inline-block; background: #2c7be5; color: #fff; border-radius: 12px; padding: 2px 10px; font-size: 0.8em;">base feature</span>
  </div>

  <p style="color: #444;">This is the initial release of the software, which includes the following features:</p>

  <h3 style="color: #2c7be5; border-bottom: 1px solid #d0e0f5; padding-bottom: 4px;">&#x2728; Base Features</h3>
  <ul style="line-height: 1.8; color: #333;">
    <li>Simulation of PK/PD target attainment for beta-lactam antibiotics in critically ill patients
      <ul>
        <li>Support for multiple dosing regimens and infusion strategies</li>
        <li>Interactive plot to visualize simulation results</li>
        <li>Automatic CFR calculation when any bacterium is selected</li>
      </ul>
    </li>
    <li>Support use of free fraction</li>
    <li>Retrieve MIC distribution and bacteria from EUCAST database</li>
    <li>In-app documentation</li>
  </ul>

[//]: # (Section for supported drugs and models)
[//]: # (update models and references as new drugs are added)
[//]: # (last updated on 2026-06-17)

  <h3 style="color: #2c7be5; border-bottom: 1px solid #d0e0f5; padding-bottom: 4px;">&#x1f489; Supported Drugs &amp; Models</h3>

  <table style="width: 100%; border-collapse: collapse; font-size: 0.93em;">
    <thead>
      <tr style="background: #2c7be5; color: #fff;">
        <th style="padding: 8px 12px; text-align: left; border-radius: 4px 0 0 0;">Drug</th>
        <th style="padding: 8px 12px; text-align: left; border-radius: 0 4px 0 0;">Reference</th>
      </tr>
    </thead>
    <tbody>
      <tr style="background: #f7f9fc;">
        <td style="padding: 7px 12px; font-weight: bold; vertical-align: top; white-space: nowrap;" rowspan="3">Piperacillin-Tazobactam</td>
        <td style="padding: 7px 12px;"><a href="https://journals.asm.org/doi/10.1128/aac.02556-19" target="_blank">Klastrup et al. JAC, 2020</a></td>
      </tr>
      <tr style="background: #f7f9fc;">
        <td style="padding: 7px 12px;"><a href="https://pubmed.ncbi.nlm.nih.gov/30963365/" target="_blank">Sukarnjanaset et al. JPP, 2019</a></td>
      </tr>
      <tr style="background: #f7f9fc;">
        <td style="padding: 7px 12px;"><a href="https://pubmed.ncbi.nlm.nih.gov/25632974/" target="_blank">Udy et al. 2015</a></td>
      </tr>
      <tr>
        <td style="padding: 7px 12px; font-weight: bold; vertical-align: top; white-space: nowrap;" rowspan="2">Cefepime</td>
        <td style="padding: 7px 12px;"><a href="https://pubmed.ncbi.nlm.nih.gov/37071586/" target="_blank">An et al. JAC, 2023</a></td>
      </tr>
      <tr>
        <td style="padding: 7px 12px;"><a href="https://pubmed.ncbi.nlm.nih.gov/37882514/" target="_blank">Barreto et al. AAC, 2023</a></td>
      </tr>
      <tr style="background: #f7f9fc;">
        <td style="padding: 7px 12px; font-weight: bold; vertical-align: top; white-space: nowrap;" rowspan="3">Ceftazidime</td>
        <td style="padding: 7px 12px;"><a href="https://www.mdpi.com/2079-6382/10/6/612" target="_blank">Buning et al. Antibiotics, 2021</a></td>
      </tr>
      <tr style="background: #f7f9fc;">
        <td style="padding: 7px 12px;"><a href="https://www.mdpi.com/2079-6382/13/8/756" target="_blank">Launay et al. Antibiotics, 2024</a></td>
      </tr>
      <tr style="background: #f7f9fc;">
        <td style="padding: 7px 12px;"><a href="https://pubmed.ncbi.nlm.nih.gov/39159014/" target="_blank">Cojutti et al. JAC, 2024</a></td>
      </tr>
      <tr>
        <td style="padding: 7px 12px; font-weight: bold; vertical-align: top; white-space: nowrap;" rowspan="6">Meropenem</td>
        <td style="padding: 7px 12px;"><a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC8754504/" target="_blank">Gijsen et al. IDR, 2021</a></td>
      </tr>
      <tr>
        <td style="padding: 7px 12px;"><a href="https://pubmed.ncbi.nlm.nih.gov/29425283/" target="_blank">Minichmayr et al. JAC, 2018</a></td>
      </tr>
      <tr>
        <td style="padding: 7px 12px;"><a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC9951903/" target="_blank">Ehrmann et al. IJAA, 2019</a></td>
      </tr>
      <tr>
        <td style="padding: 7px 12px;"><a href="https://jpharmsci.org/article/S0022-3549(24)00420-9/abstract/" target="_blank">Huang et al. 2025</a></td>
      </tr>
      <tr>
        <td style="padding: 7px 12px;"><a href="https://pubmed.ncbi.nlm.nih.gov/36253888/" target="_blank">Fukumoto et al. 2023</a></td>
      </tr>
      <tr>
        <td style="padding: 7px 12px;"><a href="https://pubmed.ncbi.nlm.nih.gov/35090867/" target="_blank">Lan et al. JPS, 2022</a></td>
      </tr>
      <tr style="background: #f7f9fc;">
        <td style="padding: 7px 12px; font-weight: bold; vertical-align: top; white-space: nowrap;" rowspan="2">Ceftolozane-Tazobactam</td>
        <td style="padding: 7px 12px;"><a href="https://pubmed.ncbi.nlm.nih.gov/25196976/" target="_blank">Chandorkar et al. ACCP, 2015</a></td>
      </tr>
      <tr style="background: #f7f9fc;">
        <td style="padding: 7px 12px;"><a href="https://ccforum.biomedcentral.com/articles/10.1186/s13054-021-03773-5" target="_blank">Zhang et al. ACCP, 2021</a></td>
      </tr>
      <tr>
        <td style="padding: 7px 12px; font-weight: bold; vertical-align: top; white-space: nowrap;">Ceftiderocol</td>
        <td style="padding: 7px 12px;"><a href="https://pubmed.ncbi.nlm.nih.gov/" target="_blank">Zhar et al. 2022</a></td>
      </tr>
    </tbody>
  </table>

</div>